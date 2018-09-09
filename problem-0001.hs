answer = sum [ x | x <- [3..999], x `mod` 3 == 0 || x `mod` 5 == 0 ]


-- Application Entry Point
main :: IO ()
main = print answer