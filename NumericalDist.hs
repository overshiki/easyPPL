module NumericalDist (
    Pdf(..),
    Sampling(..),
    Marginal(..),
    Conditional(..),
) where 

class Pdf a where 
    pdf :: a -> ([Float] -> Float)

class Sampling a where 
    sampling :: a -> IO [Float]

-- by default, to the right 
class Marginal a where 
    marginal :: a -> a

-- by default, to the right 
class Conditional a where 
    conditional :: [Float] -> a -> a 