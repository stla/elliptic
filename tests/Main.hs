module Main where
import           Approx
import           Data.Complex
import           Carlson
import           Elliptic
import           Test.Tasty       (defaultMain, testGroup)
import           Test.Tasty.HUnit (assertEqual, testCase)

i :: Complex Double
i = 0.0 :+ 1.0

main :: IO ()
main = defaultMain $
  testGroup "Tests"
  [ testCase "RF value 1" $
      assertEqual ""
        (approx 12 (carlsonRF 1 2 0))
        (approx 12 1.3110287771461),

    testCase "RF value 2" $
      assertEqual ""
        (approx 12 (carlsonRF i (-i) 0))
        (approx 12 1.8540746773014),

    testCase "RF value 3" $
      assertEqual ""
        (approx 12 (carlsonRF 0.5 1 0))
        (approx 12 1.8540746773014),

    testCase "RF value 4" $
      assertEqual ""
        (approx 13 (carlsonRF (i-1) i 0))
        (approx 13 (0.79612586584234 :+ (-1.2138566698365))),

    testCase "RF value 5" $
      assertEqual ""
        (approx 13 (carlsonRF 2 3 4))
        (approx 13 0.58408284167715),

    testCase "RF value 6" $
      assertEqual ""
        (approx 12 (carlsonRF i (-i) 2))
        (approx 12 1.0441445654064),

    testCase "RF value 7" $
      assertEqual ""
        (approx 13 (carlsonRF (i-1) i (1-i)))
        (approx 13 (0.93912050218619 :+ (-0.53296252018635)))


  ]
