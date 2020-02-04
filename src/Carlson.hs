module Carlson
  where
import Data.Complex
import Internal

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
  if zeros > 1
    then error "At most one of x, y, z can be 0"
    else
      let ((dx,dy,dz), a) = rf_ x y z err in
      let (e2,e3) = (dx*dy + dy*dz + dz*dx, dx*dy*dz) in
      toCplx(1 - e2/10 + e3/14 + e2*e2/24 - 3*e2*e3/44 - 5*e2*e2*e2/208 +
          3*e3*e3/104 + e2*e2*e3/16) / sqrt a
  where
    zeros = sum (map (\u -> fromEnum (u == 0)) [x,y,z])

carlsonRF :: Cplx -> Cplx -> Cplx -> Cplx
carlsonRF = carlsonRF' 1e-15

rd_ :: Cplx -> Cplx -> Cplx -> Cplx -> Cplx -> Double ->
       ((Double,Double,Double), Cplx, Cplx, Cplx)
rd_ x y z s fac err =
  let a = (x+y+z+z+z)/5 in
  let delta = map (\u -> magnitude(1-u/a)) [x,y,z] in
  if maximum delta < err
    then ((delta !! 0, delta !! 1, delta !! 2), a, s, fac)
    else
      let (sqrtx, sqrty, sqrtz) = (sqrt x, sqrt y, sqrt z) in
      let lambda = sqrtx*sqrty + sqrty*sqrtz + sqrtz*sqrtx in
      let s' = s + fac / (sqrt z * (z + lambda)) in
      rd_ ((x+lambda)/4) ((y+lambda)/4) ((z+lambda)/4) s' (fac/4) err

carlsonRD' :: Double -> Cplx -> Cplx -> Cplx -> Cplx
carlsonRD' err x y z =
  if zeros > 1
    then error "At most one of x, y, z can be 0"
    else
      let ((dx,dy,dz), a, s, fac) = rd_ x y z 0 1 err in
      let
        (e2,e3,e4,e5) = (dx*dy + dy*dz + 3*dz*dz + 2*dz*dx + dx*dz + 2*dy*dz,
          dz*dz*dz + dx*dz*dz + 3*dx*dy*dz + 2*dy*dz*dz + dy*dz*dz + 2*dx*dz*dz,
          dy*dz*dz*dz + dx*dz*dz*dz + dx*dy*dz*dz + 2*dx*dy*dz*dz,
          dx*dy*dz*dz*dz) in
      3*s + fac * toCplx(1 - 3*e2/14 + e3/6 + 9*e2*e2/88 - 3*e4/22 - 9*e2*e3/52 +
        3*e5/26 - e2*e2*e2/16 + 3*e3*e3/40 + 3*e2*e4/20 + 45*e2*e2*e3/272 -
        9*(e3*e4 + e2*e5)/68) / a / sqrt a
  where
    zeros = sum (map (\u -> fromEnum (u == 0)) [x,y,z])

carlsonRD :: Cplx -> Cplx -> Cplx -> Cplx
carlsonRD = carlsonRD' 1e-15
