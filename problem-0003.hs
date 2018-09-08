factors :: Integer -> [Integer]
factors n = [ x | x <- [1..n], n `mod` x == 0 ]

factorization :: Integer -> [Integer]
factorization 1 = []
factorization n = let x = (factors n) !! 1 in x : factorization (n `div` x)

answer = maximum $ factorization 600851475143

main :: IO ()
main = print answer