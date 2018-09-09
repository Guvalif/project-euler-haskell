factors :: Integer -> [Integer]
factors n = [ x | x <- [ 1 .. n ], n `mod` x == 0 ]


factorize :: Integer -> [Integer]
factorize 1 = []
factorize n = let x = (factors n) !! 1 in x : factorize (n `div` x)


answer = maximum $ factorize 600851475143


-- Application Entry Point
main :: IO ()
main = print answer