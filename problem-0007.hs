isPrime :: Integer -> Bool
isPrime n =
    let sieve n'
            | n' * n' > n     = True
            | n `rem` n' == 0 = False
            | otherwise       = sieve (n' + if n' == 2 then 1 else 2)
    in sieve 2


primes = filter isPrime [ 2.. ]
answer = primes !! 10001


-- Application Entry Point
main :: IO ()
main = print answer