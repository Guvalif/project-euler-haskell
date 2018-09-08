import Data.List (sort)

products = reverse $ sort [ x * y | x <- [100..999], y <- [100..999] ]
answer = head $ dropWhile (\x -> (show x) /= ((reverse . show) x)) products

main :: IO ()
main = print answer