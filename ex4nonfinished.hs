euclides :: (Ord a, Eq a, Num a) => a -> a -> a
euclides 0 _ = 0
euclides _ 0 = 0
euclides num1 num2
    | num1 > num2 = 1 + euclides (num1 - num2) num2
    | otherwise  = 1 + euclides num1 (num2 - num1)
