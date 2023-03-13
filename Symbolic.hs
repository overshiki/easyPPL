module Symbolic () where 

import NaiveTensor.NTensor
import NaiveTensor.Matrix
import NaiveTensor.Utils
import NumericalDist
import GHC.Float (int2Float)

-- mean vector 
type Mean = [Float]

-- covariance matrix 
type CoVar = NaiveTensor Float
data Gaussian = Gaussian Mean CoVar deriving (Show, Eq)

-- drop the first dimention, i.e. to the right 
instance Marginal Gaussian where
    marginal (Gaussian (x:xs) covar) = Gaussian xs (tailMatrix covar)

-- by default, to the right 
instance Conditional Gaussian where 
    conditional data (Gaussian mean covar) 
            = 

                where 
                    l = length data
                    [ori_mean, cond_mean] = listSplitAt l mean
                    [lu,ld,ru,rd] = matrixSplitAt (l,l) covar 
                    


(.-) :: [Float] -> [Float] -> [Float]
(.-) xs ys = map func (zip xs ys)
        where 
            func (x, y) = x - y

-- TODO: implement determinate 
gaussian_pdf :: Gaussian -> [Float] -> Float 
gaussian_pdf (Gaussian mean covar) x = 
            (exp (-1.0 * ee)) / z
            where 
                squeeze x = (flatten2list x) !! 0
                -- temperory setting determinate to 1.0, implementing the real one in the future 
                determinate = 1.0
                diff = list2flatten $ x .- mean 
                ee = -0.5 * (squeeze ((diff @@ (inv covar)) @@ diff))
                l = int2Float (length mean)
                z = (2 * pi)**(l/2)

instance Pdf Gaussian where 
    pdf gx = gaussian_pdf gx




main :: IO ()
main = do 
    let x = [1..5]
        y = [2..6]
    print $ x .- y

    let mean = [0.0, 0.0]
        covar = fromList [2,2] [1,0,0,1]
        gg = Gaussian mean covar
    print $ pdf gg [1.0,1.0]

