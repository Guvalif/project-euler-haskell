import Data.Array (Array, listArray, (!), assocs)


collatz :: Integer -> Array Integer Integer
collatz n =
    let
        memo = listArray (1, n) $ 1 : map collatz' [ 2 .. n ]

        collatz' m =
            let
                m' = if even m then (m `div` 2) else (3 * m + 1)
            in
                if m' <= n then 1 + memo ! m' else 1 + collatz' m'
    in
        memo


choiceWithSndComparing :: (Integer, Integer) -> (Integer, Integer) -> (Integer, Integer)
choiceWithSndComparing t1 t2 = if snd t1 < snd t2 then t2 else t1


answer = fst $ foldr1 choiceWithSndComparing (assocs . collatz $ 999999)


-- Application Entry Point
main :: IO ()
main = print answer
