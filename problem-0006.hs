answer = (sum [ 1 .. 100 ]) ^ 2 - sum [ x ^ 2 | x <- [ 1 .. 100 ] ]


-- Application Entry Point
main :: IO ()
main = print answer
