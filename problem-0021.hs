import Data.List (subsequences, nub)


factors :: Integer -> [Integer]
factors n = [ x | x <- [ 1 .. n ], n `mod` x == 0 ]


factorize :: Integer -> [Integer]
factorize 0 = []
factorize 1 = []
factorize n = let x = (factors n) !! 1 in x : factorize (n `div` x)


divisors :: Integer -> [Integer]
divisors = nub . map product . subsequences . factorize


isAmicable :: Integer -> Bool
isAmicable n =
    let
        m  = sum . init . divisors $ n
        n' = sum . init . divisors $ m
    in
        m /= n' && n == n'


answer = sum . filter isAmicable $ [ 1 .. 10000 ]


-- Application Entry Point
main :: IO ()
main = print answer
