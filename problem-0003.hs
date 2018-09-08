factors :: Integer -> [Integer]
factors n = [ x | x <- [1..n], n `mod` x == 0 ]

factorization :: Integer -> [Integer]
factorization 1 = []
factorization x = y : factorization (x `div` y)
    where
        y = (factors x) !! 1

answer = maximum $ factorization 600851475143

main :: IO ()
main = print answer