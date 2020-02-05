module Elliptic
  where
import Carlson
import Data.Complex
import Internal

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

ellipticE' :: Double -> Cplx -> Cplx -> Cplx
ellipticE' err phi m
  | phi == 0 =
    toCplx 0
  | abs(realPart phi) <= pi/2 =
    case m of
      0 -> phi
      1 -> sin phi
      _ ->
        let sine = sin phi in
        let sine2 = sine*sine in
        let (cosine2, oneminusmsine2) = (1 - sine2, 1 - m*sine2) in
        sine * (carlsonRF' err cosine2 oneminusmsine2 1 -
          m * sine2 / 3 * carlsonRD' err cosine2 oneminusmsine2 1)
  | otherwise =
    let (phi', k) = getPhiK phi in
    2 * fromIntegral k * ellipticE' err (pi/2) m + ellipticE' err phi' m

ellipticE :: Cplx -> Cplx -> Cplx
ellipticE = ellipticE' 1e-15

ellipticPI' :: Double -> Cplx -> Cplx -> Cplx -> Cplx
ellipticPI' err phi n m
  | phi == 0 =
    toCplx 0
  | phi == pi/2 && n == 1 =
    0/0
  | phi == pi/2 && m == 0 =
    pi/2/sqrt(1-n)
  | phi == pi/2 && m == n =
    ellipticE' err (pi/2) m / (1-m)
  | phi == pi/2 && n == 0 =
    ellipticF' err (pi/2) m
  | abs(realPart phi) <= pi/2 =
    let sine = sin phi in
    let sine2 = sine*sine in
    let (cosine2, oneminusmsine2) = (1 - sine2, 1 - m*sine2) in
    sine * (carlsonRF' err cosine2 oneminusmsine2 1 +
      n * sine2 / 3 * carlsonRJ' err cosine2 oneminusmsine2 1 (1-n*sine2))
  | otherwise =
    let (phi', k) = getPhiK phi in
    2 * fromIntegral k * ellipticPI' err (pi/2) n m + ellipticPI' err phi' n m

ellipticPI :: Cplx -> Cplx -> Cplx -> Cplx
ellipticPI = ellipticPI' 1e-15

jacobiZeta' :: Double -> Cplx -> Cplx -> Cplx
jacobiZeta' err phi m =
  if m == 1
    then
      if abs(realPart phi) <= pi/2
        then sin phi
        else let (phi',_) = getPhiK phi in sin phi'
    else
      ellipticE' err phi m -
        ellipticE' err (pi/2) m / ellipticF' err (pi/2) m *
        ellipticF' err phi m

jacobiZeta :: Cplx -> Cplx -> Cplx
jacobiZeta = jacobiZeta' 1e-15
