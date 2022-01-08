somar :: Int -> Int
somar 0 = 0
somar numero = numero + somar (numero - 1)
