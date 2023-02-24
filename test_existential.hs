-- {-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ExplicitForAll #-}
-- {-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ExistentialQuantification #-}


data NDSupport = forall a. (Eq a, Show a) => NDSupport a 

instance Show NDSupport where 
    show (NDSupport x) = show x


main :: IO ()
main = do 
    let nd = NDSupport 10 
    print nd