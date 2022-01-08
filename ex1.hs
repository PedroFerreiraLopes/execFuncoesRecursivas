factorialOld :: Int -> Int
factorialOld 0 = 1
factorialOld numero = numero * factorialOld (numero - 1)

factorialNew :: Int -> Int
factorialNew 0 = factorialOld 0
factorialNew numero 
    | numero > 0 = factorialOld numero
    | otherwise = 0


-- Apenas ao comparar ambas as fórmulas (cuja factorialNew apenas possui uma condicional a mais) já é possível
-- compreender que a factorialOld por si não consegue resolver seu valor de entrada, para qualquer valor negativo,
-- resultando em stackOverflow
