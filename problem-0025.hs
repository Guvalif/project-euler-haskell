fib :: (Integer, Integer) -> (Integer, Integer)
fib (n, n') = (n', n + n')


fibs   = iterate fib (0, 1)
answer = (+ 1) . length $ takeWhile ((< 1000) . length . show . snd) fibs


-- Application Entry Point
main :: IO ()
main = print answer