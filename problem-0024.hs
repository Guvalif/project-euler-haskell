import Data.List (permutations, sort)


answer = (!! 999999) . sort . permutations $ [ '0' .. '9' ]


-- Application Entry Point
main :: IO ()
main = print answer
