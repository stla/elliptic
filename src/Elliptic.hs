module Elliptic
  where
import Carlson
import Data.Complex
import Internal

getPhiK :: Cplx -> (Cplx, Int)
getPhiK phi
  | realPart phi > pi/2 =
    until (\(phi,_) -> realPart phi <= pi/2) (\(phi,k) -> (phi-pi,k+1)) (phi,0)
  | realPart phi < -pi/2 =
    until (\(phi,_) -> realPart phi >= -pi/2) (\(phi,k) -> (phi+pi,k-1)) (phi,0)
  | otherwise = (phi,0)

ellipticF' :: Double -> Cplx -> Cplx -> Cplx
ellipticF' err phi m
  | phi == 0 =
    toCplx 0
  | m == 1 && abs(realPart phi) == pi/2 =
    toCplx (0/0)
  | m == 1 && abs(realPart phi) < pi/2 =
    atanh(sin phi)
  | abs(realPart phi) <= pi/2 =
    if m == 0
      then
        phi
      else
        let sine = sin phi in
        let sine2 = sine*sine in
        let (cosine2, oneminusmsine2) = (1 - sine2, 1 - m*sine2) in
        sine * carlsonRF' err cosine2 oneminusmsine2 1
  | otherwise =
    let (phi', k) = getPhiK phi in
    2 * fromIntegral k * ellipticF' err (pi/2) m + ellipticF' err phi' m

ellipticF :: Cplx -> Cplx -> Cplx
ellipticF = ellipticF' 1e-15 
