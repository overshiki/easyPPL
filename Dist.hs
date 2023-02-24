import System.Random
import Utils
import NaiveTensor.NTensor

data Df a = Discrete [a] [Float] | Continuous (a->Float)

instance (Show a) => Show (Df a) where 
    show (Discrete x y) = "Discrete " ++ (show x) ++ " " ++ (show y)
    show (Continuous func) = "Continuous_func"

rand_uniform :: IO Float
rand_uniform = do 
        stdgen <- newStdGen
        let (p, nstdgen) = randomR (0.0, 1.0) stdgen
        setStdGen nstdgen
        return p


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
    nsampling :: Int -> dist a -> IO [a]

    nsampling x da = sequence (map (\x -> sampling da) [1..x])

    -- sampling da = samplingByInv (cdf da)
    -- nsampling x da = nsamplingByInv x (cdf da)



-- Categorical distribution 
data Categorical a = Categorical [a] [Float] deriving (Show, Eq)


data CatCdf a = CatCdf (Df a) deriving (Show)
instance Invertible CatCdf where 
    inverse (CatCdf (Discrete sup prob)) p = sup !! index
        where 
            index = find_index p prob


accumulative :: [Float] -> [Float]
accumulative (x1:x2:xs) = x1:(accumulative ((x1+x2):xs))
accumulative [x] = [x]

instance Distribution Categorical where
    pdf (Categorical sup prob) = Discrete sup prob
    cdf (Categorical sup prob) = Discrete sup nprob 
            where 
                nprob = map (/(sum prob)) (accumulative prob) 

    sampling cat = samplingByInv (CatCdf $ cdf cat)


-- multivariate categorical distribution
-- data D2Categorical a = D2Categorical [[a]] [[Float]] deriving (Show, Eq)

-- d2sampling :: (Eq a) => (D2Categorical a) -> IO ([a])
-- d2sampling (D2Categorical sups probs) = do 
--         let xsup = sups !! 0
--             xprob = marginal probs
--             xcat = Categorical xsup xprob
--         s <- sampling xcat 
--         let index = match_index xsup s 
--             ysup = sups !! 1
--             yprob = probs !! index 
--             ycat = Categorical ysup yprob
--         y <- sampling ycat 

--         return [s, y]


data DNCategorical a = DNCategorical [DNCategorical a] [a] [Float] | D1Categorical (Categorical a) deriving (Show, Eq)

build_DNCategorical :: (Eq a) => [[a]] -> (NaiveTensor Float) -> (DNCategorical a)
build_DNCategorical [nsup] (Tensor nprob@((Leaf p):ps))
                = D1Categorical (Categorical nsup _nprob)
        where 
            _nprob = map get_content nprob

build_DNCategorical (nsup:xs) (Tensor nprob) = DNCategorical (map (build_DNCategorical xs) nprob) nsup (map tsum nprob)


dnsampling :: (Eq a) => (DNCategorical a) -> IO ([a])
dnsampling (DNCategorical dist sup prob) = do 
        let cat = Categorical sup prob 
        s <- sampling cat 
        let index = match_index sup s 
            sample = sup !! index
            nc = dist !! index
        -- (dnsampling nc)>>=(\nsamples -> (sample:nsamples))
        nsamples <- dnsampling nc 
        return (sample:nsamples)

dnsampling (D1Categorical cat) = do 
        sample <- sampling cat 
        return [sample]        


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

    -- let d2cat = D2Categorical [["a", "b"], ["c", "d"]] [[0.1, 0.1], [0.3, 0.5]]
    -- d2s <- d2sampling d2cat
    -- print d2s

    let d1cat1 = D1Categorical (Categorical ["a","b","c"] [0.6, 0.2, 0.2])
        d1cat2 = D1Categorical (Categorical ["d","e","f"] [0.3, 0.2, 0.2])
        d2cat = DNCategorical [d1cat1, d1cat2] ["g", "h"] [0.8, 0.2]
    dns <- dnsampling d2cat
    print dns

    let ntones = ones [2,2]
        d2uniform = build_DNCategorical [["a", "b"], ["c", "d"]] ntones

    dns <- dnsampling d2uniform
    print "uniform"
    print dns