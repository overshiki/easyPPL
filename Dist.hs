-- {-# LANGUAGE ScopedTypeVariables #-}
-- {-# LANGUAGE ExplicitForAll #-}
-- {-# LANGUAGE RankNTypes #-}
-- {-# LANGUAGE ExistentialQuantification #-}
-- {-# LANGUAGE InstanceSigs #-}


import System.Random
import Utils
import NaiveTensor.NTensor
import NaiveTensor.Order
import NaiveTensor.Broadcast
import NaiveTensor.Statistic


-- data Df a = Discrete [a] [Float] | Continuous (a->Float) | NotAvaliable
data Df a = Discrete (NaiveTensor a) (NaiveTensor Float) | Continuous (a->Float) | NotAvaliable

instance (Show a) => Show (Df a) where 
    show (Discrete x y) = "Discrete " ++ (show x) ++ " " ++ (show y)
    show (Continuous func) = "Continuous_func"

rand_uniform :: IO Float
rand_uniform = do 
        stdgen <- newStdGen
        let (p, nstdgen) = randomR (0.0, 1.0) stdgen
        setStdGen nstdgen
        return p

-- helper typeclass for sampling through inverse transform sampling method
class Invertible d where 
    inverse :: d a -> Float -> a 
    samplingByInv :: d a -> IO a 
    nsamplingByInv :: Int -> d a -> IO [a]
    samplingByInv da = do 
        p <- rand_uniform
        return $ inverse da p

    nsamplingByInv x da = sequence (map (\x -> samplingByInv da) [1..x]) 


class Distribution dist where 
    pdf :: dist a -> Df a
    cdf :: dist a -> Df a
    sampling :: dist a -> IO a
    direct_sampling :: dist a -> IO a
    nsampling :: Int -> dist a -> IO [a]

    pdf x = NotAvaliable
    cdf x = NotAvaliable
    direct_sampling = sampling
    nsampling x da = sequence (map (\x -> sampling da) [1..x])


-- Categorical distribution 
data Categorical a = Categorical [a] [Float] deriving (Show, Eq)


data CatCdf a = CatCdf (Df a) deriving (Show)
instance Invertible CatCdf where 
    inverse (CatCdf (Discrete (Tensor sup) prob)) p = get_content $ sup !! index
        where 
            index = (find_inBetween_index p prob) !! 0

instance Distribution Categorical where
    pdf (Categorical sup prob) = Discrete (totensor sup) (totensor prob)
            where 
                totensor xs = Tensor (map Leaf xs)
    cdf (Categorical sup prob) = Discrete (totensor sup) (accumulate (totensor prob)) 
            where 
                totensor xs = Tensor (map Leaf xs)
                -- nprob = map (/(sum prob)) (accumulative prob) 

    sampling cat = samplingByInv (CatCdf $ cdf cat)


-- multivariate categorical distribution using NaiveTensor
data NTCategorical a = NTCategorical (NaiveTensor a) (NaiveTensor Float) deriving (Show, Eq)

build_NTCategorical :: (Eq a) => [[a]] -> (NaiveTensor Float) -> (NTCategorical [a])
build_NTCategorical sup prob = NTCategorical nsup prob
        where 
            nsup = cartesian sup

ntsampling :: (NTCategorical a) -> IO ([Int])
ntsampling (NTCategorical (Tensor sup) (Tensor prob@((Tensor x):xs))) = do
                        index <- sampling dist 
                        let nprob = prob !! index 
                            nsup = sup !! index
                            nnt = NTCategorical nsup nprob
                        nindices <- ntsampling nnt 
                        return (index:nindices)                                                
                        where 
                            nprob = map tsum prob
                            dist = Categorical (rangelike nprob) nprob

ntsampling (NTCategorical sup (Tensor prob@((Leaf x):xs))) = do 
                        index <- sampling dist 
                        return [index]
                        where 
                            nprob = map get_content prob 
                            dist = Categorical (rangelike nprob) nprob


data NTCatCdf a = NTCatCdf (Df a) deriving (Show)
instance Invertible NTCatCdf where 
    inverse (NTCatCdf (Discrete sup prob)) p = tselect indices sup
        where 
            indices = find_inBetween_index p prob

check_inBetween_index :: (NTCategorical a) -> IO ([Int])
check_inBetween_index nt = do 
            p <- rand_uniform
            print p
            let (Discrete sup prob) = cdf nt 
            print prob
            return $ find_inBetween_index p prob


normalize :: (Df a) -> (Df a)
normalize (Discrete sup prob) = Discrete sup (_normalize prob)
        where 
            _normalize nt = fmap (/(nt_max nt)) nt

instance Distribution NTCategorical where 
    pdf (NTCategorical sup prob) = Discrete sup prob
    cdf (NTCategorical sup prob) = normalize $ Discrete sup (accumulate prob)
    sampling nt@(NTCategorical sup prob) = do 
        indices <- ntsampling nt 
        return $ tselect indices sup
    
    direct_sampling cat = samplingByInv (NTCatCdf $ cdf cat)




-- Gaussian distribution
type Mean = Float
type Std = Float 
data Gaussian a = Gaussian (Float->a) Mean Std 

instance (Show a) => Show (Gaussian a) where 
    show (Gaussian supfunc m std) = "Gaussian " ++ (show m) ++ " " ++ (show std)

boxMuller :: IO Float
boxMuller = do 
        u1 <- rand_uniform
        u2 <- rand_uniform
        let u = (sqrt ((-1) * (log (u1)))) * (sin (2 * pi * u2))
        return u

instance Distribution Gaussian where
    sampling (Gaussian supfunc m std) = do 
        u <- boxMuller
        return (supfunc ((u+m) * std))


main :: IO ()
main = do 
    let cat = Categorical ["a","b","c"] [0.6, 0.2, 0.2]
    print $ pdf cat
    print $ cdf cat
    let (Discrete sup prob) = cdf cat 
    print $ prob

    s <- samplingByInv (CatCdf (cdf cat)) 
    print s
    xs <- nsamplingByInv 10 (CatCdf (cdf cat)) 
    print xs

    s <- sampling cat 
    print s 
    xs <- nsampling 10 cat 
    print xs


    let d = NTCategorical (ones [2,2]) ((ones [2,2])::(NaiveTensor Float))
    s <- ntsampling d 
    print s
    s <- sampling d 
    print s

    let d = build_NTCategorical [["a","b"], ["c", "d"]] ((ones [2,2])::(NaiveTensor Float))
    print d
    s <- sampling d 
    print s
    print d
    print $ cdf d
    s <- check_inBetween_index d 
    print s
    s <- direct_sampling d 
    print s
    s <- boxMuller
    print s 
    s <- boxMuller
    print s

    let d = Gaussian id 0.0 1.0 
    s <- sampling d 
    print s 
    s <- nsampling 10 d 
    print s