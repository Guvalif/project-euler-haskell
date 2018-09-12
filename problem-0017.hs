word :: Int -> String
word n
    | 0 < n && n <= 12   = words_1_to_12 !! (n - 1)
    | 12 < n && n <= 19  = (words_modify !! ((n `mod` 10) - 2)) ++ "teen"
    | 19 < n && n <= 99  = (words_modify' !! ((n `div` 10) - 2)) ++ "ty" ++ word (n `mod` 10)
    | 99 < n && n <= 999 = (words_1_to_12 !! ((n `div` 100) - 1)) ++ "hundred" ++ (if n `mod` 100 /= 0 then "and" else "") ++ word (n `mod` 100)
    | n == 1000          = "onethousand"
    | otherwise          = ""
    where
        words_1_to_12 = [ "one", "two", "three", "four", "five", "six", "seven", "eight", "nine", "ten", "eleven", "twelve" ]
        words_modify  = [ "twen", "thir", "four", "fif", "six", "seven", "eigh", "nine" ]
        words_modify' = [ "twen", "thir", "for", "fif", "six", "seven", "eigh", "nine" ]


answer = sum $ map (length . word) [ 1 .. 1000 ]


-- Application Entry Point
main :: IO ()
main = print answer