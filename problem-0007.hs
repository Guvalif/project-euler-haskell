-- This program runs too slow! It should be refactored X(

factors :: Integer -> [Integer]
factors n = [ x | x <- [ 2 .. n - 1 ], n `mod` x == 0 ]


is_prime :: Integer -> Bool
is_prime n = case factors n of
    []     -> True
    x : xs -> False


primes = filter is_prime [ 2.. ]
answer = head . drop 10000 $ primes


-- Application Entry Point
main :: IO ()
main = print answer