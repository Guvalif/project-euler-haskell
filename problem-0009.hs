pythagorean_triples = [ [a, b, 1000 - a - b] | a <- [ 1 .. 1000 ], b <- [ 1 .. 1000 ], a^2 + b^2 == (1000 - a - b)^2 ]
answer              = product $ head pythagorean_triples


-- Application Entry Point
main :: IO ()
main = print answer