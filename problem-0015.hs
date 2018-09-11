fact :: Int -> Integer
fact n = product $ take n [ 1 .. ]


paths :: Int -> Integer
paths n = fact (2 * n) `div` (fact n) ^ 2


answer = paths 20


-- Application Entry Point
main :: IO ()
main = print answer