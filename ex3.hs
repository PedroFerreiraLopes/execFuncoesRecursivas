(^^^) :: (Num a, Eq a) => a -> a -> a
_ ^^^ 0 = 1
base ^^^ expoente = base * (base ^^^ (expoente - 1))

-- ^^^ É a nova implementação de exponenciação
