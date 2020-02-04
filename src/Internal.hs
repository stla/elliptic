module Internal
  where
import Data.Complex

type Cplx = Complex Double

toCplx :: Double -> Cplx
toCplx x = x :+ 0.0

atanC :: Cplx -> Cplx
atanC z = i * (log(1-i*z) - log(1+i*z)) / 2
  where
    i = 0.0 :+ 1.0
