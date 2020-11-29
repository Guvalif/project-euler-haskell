digitsSum :: Integer -> Integer
digitsSum = sum . map (read . (: [])) . show


answer = digitsSum $ 2 ^ 1000


-- Application Entry Point
main :: IO ()
main = print answer
