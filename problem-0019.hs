data DayW  = Mon | Tue | Wed | Thu | Fri | Sat | Sun deriving (Eq, Ord, Show, Enum)
data Month = Jan | Feb | Mar | Apr | May | Jun | Jul | Aug | Sep | Oct | Nov | Dec deriving (Eq, Ord, Show, Enum)
data Date  = Date { year :: Integer, month :: Month, day :: Integer, dayw :: DayW } deriving (Eq, Ord, Show)


nextDayW :: DayW -> DayW
nextDayW Sun  = Mon
nextDayW dayw = toEnum . (+ 1) . fromEnum $ dayw


nextMonth :: Month -> Month
nextMonth Dec   = Jan
nextMonth month = toEnum . (+ 1) . fromEnum $ month


isLeapYear :: Integer -> Bool
isLeapYear year
    | year `mod` 400 == 0 = True
    | year `mod` 100 == 0 = False
    | year `mod` 4   == 0 = True
    | otherwise           = False


nextDate :: Date -> Date
nextDate date =
    let
        next_dayw = nextDayW . dayw $ date
    in
        case date of
            Date year' Dec    31   _ -> Date (year' + 1) Jan 1 next_dayw
            Date year' Feb    28   _ -> if isLeapYear year' then date { day = 29, dayw = next_dayw } else Date year' Mar 1 next_dayw
            Date year' Feb    29   _ -> Date year' Mar 1 next_dayw
            Date year' Apr    30   _ -> Date year' May 1 next_dayw
            Date year' Jun    30   _ -> Date year' Jul 1 next_dayw
            Date year' Sep    30   _ -> Date year' Oct 1 next_dayw
            Date year' Nov    30   _ -> Date year' Dec 1 next_dayw
            Date year' month' 31   _ -> Date year' (nextMonth month') 1 next_dayw
            Date _     _      day' _ -> date { day = day' + 1, dayw = next_dayw }


isSunOn1st :: Date -> Bool
isSunOn1st (Date _ _ 1 Sun) = True
isSunOn1st _                = False


dates     = iterate nextDate $ Date 1900 Jan 1 Mon
century21 = takeWhile ((<= 2000) . year) $ dropWhile ((== 1900) . year) dates
answer    = length $ filter isSunOn1st century21


-- Application Entry Point
main :: IO ()
main = print answer