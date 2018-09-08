fibs = 1 : 2 : zipWith (+) fibs (tail fibs)
answer = sum [ x | x <- takeWhile (< 4000000) fibs, x `mod` 2 == 0 ]

main :: IO ()
main = print answer