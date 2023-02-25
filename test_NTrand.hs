import NaiveTensor.NTRand

main :: IO ()
main = do 
    x <- randn [2,2,2]
    print x