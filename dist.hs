import System.Random

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

-- TODO: more efficient implementation 
find_index :: Float -> [Float] -> Int 
find_index target (x:xs)
        | target < x  = 0 + (find_index target xs)
        | target >= x = 1 + (find_index target xs)
find_index target [] = 0

data Cdf a = Cdf (Df a) deriving (Show)
instance Invertible Cdf where 
    inverse (Cdf (Discrete sup prob)) p = sup !! index
        where 
            index = find_index p prob


data Categorical a = Categorical [a] [Float] deriving (Show, Eq)


accumulative :: [Float] -> [Float]
accumulative (x1:x2:xs) = x1:(accumulative ((x1+x2):xs))
accumulative [x] = [x]


instance Distribution Categorical where
    pdf (Categorical sup prob) = Discrete sup prob
    cdf (Categorical sup prob) = Discrete sup nprob 
            where 
                nprob = map (/(sum prob)) (accumulative prob) 

    sampling cat = samplingByInv (Cdf $ cdf cat)



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

    s <- samplingByInv (Cdf (cdf cat)) 
    print s
    xs <- nsamplingByInv 10 (Cdf (cdf cat)) 
    print xs

    s <- sampling cat 
    print s 
    xs <- nsampling 10 cat 
    print xs