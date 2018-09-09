isPrime :: Integer -> Bool
isPrime n =
    let sieve n'
            | n' * n' > n     = True
            | n `mod` n' == 0 = False
            | otherwise       = sieve (n' + if n' == 2 then 1 else 2)
    in sieve 2


primes  = filter isPrime [ 2 .. ]
primes' = takeWhile (< (truncate . sqrt $ 2000000)) primes


isPrime' :: Integer -> Bool
isPrime' n
    | n <= last primes' = any (== n) primes'
    | otherwise         = all (\p -> n `mod` p /= 0) primes'


answer = sum $ filter isPrime' [ 2 .. 2000000 ]


-- Application Entry Point
main :: IO ()
main = print answer