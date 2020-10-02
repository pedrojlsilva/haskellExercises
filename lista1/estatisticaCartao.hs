type Palavra = String
separadores = ";"

ehSeparador :: Char -> String -> Bool
ehSeparador c "" = False
ehSeparador c (a:as)
  | c == a    = True
  | otherwise = ehSeparador c as 

getPalavra :: String -> Palavra
getPalavra "" = ""
getPalavra (c:cs) 
  | ehSeparador c separadores  = ""
  | otherwise = c:getPalavra cs

dropPalavra :: String -> String
dropPalavra "" = ""
dropPalavra texto@(c:cs)
  | not (ehSeparador c separadores)  = dropPalavra cs
  | otherwise = texto

dropEspaco :: String -> String
dropEspaco "" = ""
dropEspaco texto@(c:cs) 
  | ehSeparador c separadores  = dropEspaco cs
  | otherwise = texto

splitPalavras :: String -> [Palavra]
splitPalavras "" = []
splitPalavras texto = 
    let textoOk = dropEspaco texto
    in (getPalavra textoOk) : splitPalavras (dropEspaco (dropPalavra textoOk))

getValorAux [] idx = []
getValorAux str idx
    |idx == 2 =  head str: getValorAux (tail str) (0)
    |otherwise = getValorAux (tail str) (idx+1)


getValor str = getValorAux (splitPalavras str) 0

stringToDouble :: [String] -> [Double]
stringToDouble [] = []
stringToDouble (x:xs) = (read x :: Double) : stringToDouble xs

strToDouble str = stringToDouble(getValor str)

maiorLista :: [Double] -> Double
maiorLista (x:[]) = x
maiorLista (x:xs)
  | x > maiorLista xs = x
  | otherwise = maiorLista xs


menorLista :: [Double] -> Double
menorLista (x:[]) = x
menorLista (x:xs)
  | x < menorLista xs = x
  | otherwise = menorLista xs

minMaxCartao :: String -> (Double, Double)
minMaxCartao a = (menorLista(strToDouble a),maiorLista(strToDouble a))

main = do
    a <- getLine
    let result = minMaxCartao a
    print result