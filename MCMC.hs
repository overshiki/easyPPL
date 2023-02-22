

class MCMC a where
    propose :: a -> IO a
    mh_correct :: a -> Bool
    logp :: a -> Float
    