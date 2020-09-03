
imc :: Float -> Float -> Float
imc peso altura = peso/(altura * altura)

{- altura^2
-}
primoAuxiliar :: Int -> Int -> Bool
primoAuxiliar n divisor | divisor > (n `div` 2) = True
                        | n `mod` divisor == 0  = False
                        | otherwise = primoAuxiliar n (divisor+1)


primo :: Int ->  Bool
primo n = if n < 2
          then False
          else primoAuxiliar n 2

todosOsPrimos :: Int -> [Int]
todosOsPrimos n = reverse (todosOsPrimosAux n)

todosOsPrimosAux :: Int -> [Int] {-lista de inteiros-}
todosOsPrimosAux n | n < 2     = []
                | primo n   = n : todosOsPrimosAux (n-1) {- cabeça da lista : cauda da lista -}
                | otherwise = todosOsPrimosAux (n-1)