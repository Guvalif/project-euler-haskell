fact :: Int -> Integer
fact n = product $ take n [ 1 .. ]


digitsSum :: Integer -> Integer
digitsSum = sum . map (read . (: [])) . show


answer = digitsSum . fact $ 100


-- Application Entry Point
main :: IO ()
main = print answer