factors :: Integer -> [Integer]
factors n = [ x | x <- [1..n], n `mod` x == 0 ]


factorization :: Integer -> [Integer]
factorization 1 = []
factorization n = let x = (factors n) !! 1 in x : factorization (n `div` x)


count :: Integer -> [Integer] -> (Integer, Integer)
count n xs = (n, toInteger $ length (filter (== n) xs))


factor_counts :: [Integer] -> [Integer] -> [(Integer, Integer)]
factor_counts xs primes = [ f xs | f <- map count primes ]


max_dim :: Integer -> [(Integer, Integer)] -> Integer
max_dim n xs= maximum (map snd $ filter (\t -> fst t == n) xs)


factors_2_to_20   = map factorization [2..20]
primes_2_to_20    = concat $ filter ((== 1) . length) factors_2_to_20
all_factor_counts = filter (\t -> snd t /= 0) $ concatMap (\f -> factor_counts f primes_2_to_20) factors_2_to_20

answer = product $ zipWith (^) primes_2_to_20 (map (\p -> max_dim p all_factor_counts) primes_2_to_20)


-- Application Entry Point
main :: IO ()
main = print answer