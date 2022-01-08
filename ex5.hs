boolAnd :: [Bool] -> Bool 
boolAnd [] = True
boolAnd (first : rest)
    | not first = False 
    | length (first : rest) == 1 = True 
    | otherwise = first && boolAnd rest


newConcat :: [[a]] -> [a]
newConcat [] = []
newConcat (first : rest) = first ++ newConcat rest


newReplicate :: Int -> a -> [a]
newReplicate 0 _ = []
newReplicate reps value = value : newReplicate (reps - 1) value


newSelect :: [a] -> Int -> Maybe a
newSelect [] _ = Nothing 
newSelect list@(first : rest) position
    | position == 0 = Just first
    | position < 0 = Nothing
    | otherwise = newSelect rest (position - 1)

newElem :: Eq a => a -> [a] -> Bool 
newElem _ [] = False
newElem element (first : rest)
    | element == first = True
    | otherwise = newElem element rest
