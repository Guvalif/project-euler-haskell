import Control.Applicative (liftA2)
import Data.Maybe (catMaybes)
import Data.List (group)
import Data.Set (fromList, difference)


primes =
    let
        f (p : ps) = p : (filter $ (/= 0) . (`mod` p)) ps
    in
        f [ 2 .. ]


primeFactors :: Integer -> [Integer]
primeFactors n = [ x | x <- takeWhile (\y -> y * y <= n) primes, n `mod` x == 0 ]


factorize :: Integer -> [Integer]
factorize 1 = []
factorize n =
    let
        x = case primeFactors n of
            []     -> n
            p : ps -> p
    in
        x : factorize (n `div` x)


powers :: [Integer] -> [Integer]
powers = foldr (\x -> \acc -> head acc * x : acc) [ 1 ]


factors :: Integer -> [Integer]
factors = foldr (liftA2 (*)) [ 1 ] . fmap powers . group . factorize


isAbundantNumber :: Integer -> Maybe Integer
isAbundantNumber n =
    let
        s' = sum . factors $ n
        s  = s' - n
    in
        if s <= n then Nothing else Just n

-- Application Entry Point
main :: IO ()
main = do
    let abundants               = catMaybes . fmap isAbundantNumber $ [ 1 .. 28123 ]
    let composite_abundants     = fromList ((+) <$> abundants <*> abundants)
    let non_composite_abundants = difference (fromList [ 1 .. 28123 ]) composite_abundants

    print . sum $ non_composite_abundants
