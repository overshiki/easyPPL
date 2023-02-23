
module Utils (find_index, marginal, match_index) where 


-- TODO: more efficient implementation 
find_index :: Float -> [Float] -> Int 
find_index target (x:xs)
        | target < x  = 0 + (find_index target xs)
        | target >= x = 1 + (find_index target xs)
find_index target [] = 0

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