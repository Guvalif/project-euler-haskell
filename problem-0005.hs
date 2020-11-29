answer = foldr lcm 1 [ 2 .. 20 ]


-- Application Entry Point
main :: IO ()
main = print answer
