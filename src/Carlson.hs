module Carlson
  where
import Data.Maybe
import Data.Complex

type Cplx = Complex Double

toCplx :: Double -> Cplx
toCplx x = x :+ 0.0

rf_ :: Cplx -> Cplx -> Cplx -> Double -> ((Double,Double,Double), Cplx)
rf_ x y z err =
  let a = (x+y+z)/3 in
  let delta = map (\u -> magnitude(1-u/a)) [x,y,z] in
  if maximum delta < err
    then ((delta !! 0, delta !! 1, delta !! 2), a)
    else
      let (sqrtx, sqrty, sqrtz) = (sqrt x, sqrt y, sqrt z) in
      let lambda = sqrtx*sqrty + sqrty*sqrtz + sqrtz*sqrtx in
      rf_ ((x+lambda)/4) ((y+lambda)/4) ((z+lambda)/4) err

carlsonRF' :: Double -> Cplx -> Cplx -> Cplx -> Cplx
carlsonRF' err x y z =
  let ((dx,dy,dz), a) = rf_ x y z err in
  let (e2,e3) = (dx*dy + dy*dz + dz*dx, dx*dy*dz) in
  toCplx(1 - e2/10 + e3/14 + e2*e2/24 - 3*e2*e3/44 - 5*e2*e2*e2/208 +
      3*e3*e3/104 + e2*e2*e3/16) / sqrt a

carlsonRF :: Cplx -> Cplx -> Cplx -> Cplx
carlsonRF = carlsonRF' 1e-15
