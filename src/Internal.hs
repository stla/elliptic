module Internal
  where
import Data.Complex

type Cplx = Complex Double

toCplx :: Double -> Cplx
toCplx x = x :+ 0.0
