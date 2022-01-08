merge :: Ord a => [a] -> [a] -> [a]
merge [] second = second
merge first [] = first
merge (fFirst : fRest) (sFirst : sRest)
    | fFirst >= sFirst = sFirst : merge (fFirst : fRest) sRest
    | sFirst > fFirst = fFirst : merge fRest (sFirst : sRest)
