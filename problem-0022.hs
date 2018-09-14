import Data.List (sort)
import Data.Char (toLower)


position :: Char -> Integer
position c = toInteger . length $ [ 'a' .. toLower c ]


prettify :: Char -> Char
prettify '"' = ' '
prettify ',' = ' '
prettify c   = c


-- Application Entry Point
main :: IO ()
main = do
    contents <- readFile "problem-0022_names.txt"

    let names   = sort . words . map prettify $ contents
    let scores' = map (sum . map position) names
    let scores  = zipWith (*) scores' [ 1 .. ]

    print $ sum scores