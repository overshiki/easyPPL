
module Utils (find_index, 
                marginal, 
                match_index, 
                unflatten,
                accumulative,
                rangelike
                ) where 

-- TODO: more efficient implementation 
find_index :: Float -> [Float] -> Int 
find_index target (x:xs)
        | target < x  = 0
        | target >= x = 1 + (find_index target xs)
find_index target [] = 0

accumulative :: [Float] -> [Float]
accumulative (x1:x2:xs) = x1:(accumulative ((x1+x2):xs))
accumulative [x] = [x]

slice :: (Eq a) => [Int] -> [a] -> [a]
slice inds values = map (values !!) inds

rangelike :: [a] -> [Int]
rangelike targets = [0.._len]
        where 
                _len = (length targets) - 1


lsplitAt :: (Eq a) => Int -> [a] -> [[a]]
lsplitAt n xs = [h, t]
        where 
                h = slice [0..(n-1)] xs 
                t = slice [n .. (length xs)-1] xs


unflatten :: (Eq a) => [Int] -> [a] -> [[a]]
unflatten (i:is) values = h:(unflatten is t)
        where 
                [h, t] = lsplitAt i values
unflatten [] _ = []



marginal :: [[Float]] -> [Float]
marginal xs = map sum xs

match_index :: (Eq a) => [a] -> a -> Int 
match_index (x:xs) t
        | x==t      = 0
        | otherwise = 1 + (match_index xs t)
match_index [] t = -1


main :: IO ()
main = do 
        let index = match_index ["a", "b", "c"] "c"
        print index
        let v = unflatten [2,2] [1..4]
        print v