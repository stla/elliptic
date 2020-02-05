module Approx where
import Data.Complex

approxDbl :: Int -> Double -> Double
approxDbl n x = fromInteger (round $ x * (10^n)) / (10.0^^n)

approx :: Int -> Complex Double -> Complex Double
approx n z = approxDbl n (realPart z) :+ approxDbl n (imagPart z)
