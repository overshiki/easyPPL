-- {-# LANGUAGE ScopedTypeVariables #-}
-- {-# LANGUAGE ExplicitForAll #-}
-- {-# LANGUAGE RankNTypes #-}
-- {-# LANGUAGE ExistentialQuantification #-}

import System.Random
import Utils
import NaiveTensor.NTensor


data Df a = Discrete [a] [Float] | Continuous (a->Float) | NotAvaliable

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
    sampling :: (Eq a) => dist a -> IO a
    nsampling :: (Eq a) => Int -> dist a -> IO [a]
    nsampling x da = sequence (map (\x -> sampling da) [1..x])


-- Categorical distribution 
data Categorical a = Categorical [a] [Float] deriving (Show, Eq)


data CatCdf a = CatCdf (Df a) deriving (Show)
instance Invertible CatCdf where 
    inverse (CatCdf (Discrete sup prob)) p = sup !! index
        where 
            index = find_index p prob

instance Distribution Categorical where
    pdf (Categorical sup prob) = Discrete sup prob
    cdf (Categorical sup prob) = Discrete sup nprob 
            where 
                nprob = map (/(sum prob)) (accumulative prob) 

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
                            _len = length prob
                            nprob = map tsum prob
                            dist = Categorical [0.._len-1] nprob

ntsampling (NTCategorical sup (Tensor prob@((Leaf x):xs))) = do 
                        index <- sampling dist 
                        return [index]
                        where 
                            _len = length prob 
                            nprob = map get_content prob 
                            dist = Categorical [0.._len-1] nprob

instance Distribution NTCategorical where 
    pdf _ = NotAvaliable
    cdf _ = NotAvaliable
    sampling nt@(NTCategorical sup prob) = do 
        indices <- ntsampling nt 
        return $ tselect indices sup



    


-- -- Gaussian distribution 
-- data Gaussian = Gaussian Float Float deriving (Show, Eq)

-- instance Distribution Gaussian where
--     pdf (Gaussian mean var) = mean
--     cdf (Gaussian mean var) = NotAvaliable
--     sampling cat = boxMuller cat




main :: IO ()
main = do 
    let cat = Categorical ["a","b","c"] [0.6, 0.2, 0.2]
    print $ pdf cat
    print $ cdf cat
    let (Discrete sup prob) = cdf cat 
    print $ prob
    print $ find_index 0.3 prob
    print $ find_index 0.7 prob
    print $ find_index 0.9 prob

    s <- samplingByInv (CatCdf (cdf cat)) 
    print s
    xs <- nsamplingByInv 10 (CatCdf (cdf cat)) 
    print xs

    s <- sampling cat 
    print s 
    xs <- nsampling 10 cat 
    print xs

    -- let d1cat1 = D1Categorical (Categorical ["a","b","c"] [0.6, 0.2, 0.2])
    --     d1cat2 = D1Categorical (Categorical ["d","e","f"] [0.3, 0.2, 0.2])
    --     d2cat = DNCategorical [d1cat1, d1cat2] ["g", "h"] [0.8, 0.2]
    -- dns <- dnsampling d2cat
    -- print dns

    -- let ntones = ones [2,2]
    --     d2uniform = build_DNCategorical [["a", "b"], ["c", "d"]] ntones

    -- dns <- dnsampling d2uniform
    -- print "uniform"
    -- print dns

    -- let d = build_DN_from_NT (build_NTCategorical [["a", "b"], ["c", "d"]] ntones)
    -- print d
    -- s <- dnsampling d 
    -- print s


    -- let d = build_NTCategorical [["a", "b"], ["c", "d"]] ntones
    -- print d
    -- s <- ntsampling d 
    -- print s

    let d = NTCategorical (ones [2,2]) ((ones [2,2])::(NaiveTensor Float))
    s <- ntsampling d 
    print s
    s <- sampling d 
    print s

    let d = build_NTCategorical [["a","b"], ["c", "d"]] (ones [2,2])
    print d
    s <- sampling d 
    print s