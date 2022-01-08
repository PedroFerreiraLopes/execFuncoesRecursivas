import Data.List

metades :: [a] -> ([a], [a])
metades todo
    | mod (length todo) 2 == 0 = ([inicio | inicio <- todo, numero <- [0.. length todo]], [])

merge :: Ord a => [a] -> [a] -> [a]
merge [] second = second
merge first [] = first
merge (fFirst : fRest) (sFirst : sRest)
    | fFirst >= sFirst = sFirst : merge (fFirst : fRest) sRest
    | sFirst > fFirst = fFirst : merge fRest (sFirst : sRest)
