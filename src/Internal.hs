module Internal
  where
import Data.Complex

type Cplx = Complex Double

toCplx :: Double -> Cplx
toCplx x = x :+ 0.0

getPhiK :: Cplx -> (Cplx, Int)
getPhiK phi
  | realPart phi > pi/2 =
    until (\(x,_) -> realPart x <= pi/2) (\(x,k) -> (x-pi,k+1)) (phi,0)
  | realPart phi < -pi/2 =
    until (\(x,_) -> realPart x >= -pi/2) (\(x,k) -> (x+pi,k-1)) (phi,0)
  | otherwise = (phi,0)

atanC :: Cplx -> Cplx
atanC z = i * (log(1-i*z) - log(1+i*z)) / 2
  where
    i = 0.0 :+ 1.0
