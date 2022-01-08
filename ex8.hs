listSum :: Num a => [a] -> a
listSum [] = 0
listSum (h : hs) = h + listSum hs

listCrop :: (Eq b, Num b) => [a] -> b -> [a]
listCrop [] _ = []
listCrop _ 0 = []
listCrop (h : hs) quant = h : listCrop hs (quant - 1)

listLast :: [a] -> Maybe a
listLast lista
    | null lista = Nothing
    | length lista == 1 = Just (head lista)
    | otherwise = listLast (tail lista)