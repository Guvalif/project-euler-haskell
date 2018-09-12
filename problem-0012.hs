import Control.Arrow ((&&&))
import Data.List (group)


factors :: Integer -> [Integer]
factors n = [ x | x <- [ 1 .. n ], n `mod` x == 0 ]


factorize :: Integer -> [Integer]
factorize 1 = []
factorize n = let x = (factors n) !! 1 in x : factorize (n `div` x)


divisors :: Integer -> Integer
divisors n = product $ map ((+ 1) . toInteger . length) $ (group . factorize) n


nthTriangle :: Integer -> Integer
nthTriangle n = (n * (n + 1)) `div` 2


answer = fst . head $ dropWhile ((< 500) . snd) $ map ((id &&& divisors) . nthTriangle) [ 1 .. ]


-- Application Entry Point
main :: IO ()
main = print answer