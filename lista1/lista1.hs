--multiplicação de listas
mul2 :: [Int]->[Int]->[Int]
mul2 [][] = []
mul2 [](b:bs) = b*0 : mul2 [] bs
mul2 (a:as)[] = a*0 : mul2 as []
mul2 (a:as) (b:bs) = a*b : mul2 as bs

--binario para inteiro
btoi :: String -> Int
btoi palavra
  | palavra == [] = 0
  | head palavra == '1' = 2^((length palavra)- 1) + btoi (tail palavra)
  | otherwise = 0+btoi (tail palavra)

--Detecção de replica
isReplica :: String -> Int -> Char -> Bool
isReplica [] qt c 
    | qt > 0 = False
    |otherwise = True

isReplica str qt c
    | length str /= qt = False
    | qt == 0  = True
    | head str == c = isReplica (tail str) (qt-1) c 
    | otherwise = False


--Calculadora
type Comando = String
type Valor = Int
executaAux [] result = result
executaAux lista result
    | fst (head lista) == "Divide" && snd (head lista) == 0 = -666
    | fst (head lista) == "Multiplica" = executaAux (tail lista) (result * snd (head lista)) 
    | fst (head lista) == "Soma" = executaAux (tail lista) (result + snd (head lista))
    | fst (head lista) == "Subtrai" = executaAux (tail lista) (result - snd (head lista))
    | fst (head lista) == "Divide" = executaAux (tail lista) (result `div` snd (head lista))

executa :: [(Comando, Valor)] -> Int
executa lista = executaAux lista 0


--estatistica da fatura do cartão
type Mes = String
type Local = String
type Preco = Double
type Compra = (Mes, Local, Preco)
getPreco :: Compra->Double
getPreco (m,l,p) = p

minMaxAux :: [Compra]->(Double, Double)-> (Double, Double)


minMaxCartao :: String -> (Double, Double)
minMaxCartao compras = minMaxAux (getValues (splitPalavras compras)) ()


