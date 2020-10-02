type Palavra = String
separadores = "; "

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
    |idx == 1 =  head str: getValorAux (tail str) (idx+1)
    |idx == 3 =  head str: getValorAux (tail str) (0)
    |otherwise = getValorAux (tail str) (idx+1)


getValor str = getValorAux (splitPalavras str) 0

filterMonthAux month [] idx = []
filterMonthAux month str idx
    |idx == 0 && head str == month =  head (tail str): filterMonthAux month (tail str) (idx+1)
    |idx == 1 = filterMonthAux month (tail str) (0)
    |otherwise = filterMonthAux month (tail str) (0)


filterMonth month str = filterMonthAux month (getValor str) 0



stringToDouble :: [String] -> [Double]
stringToDouble [] = []
stringToDouble (x:xs) = (read x :: Double) : stringToDouble xs

strToDouble month str = stringToDouble(filterMonth month str)


sumList :: [Double]->Double
sumList []=0
sumList (a:as) = a + sumList as

logMes month str = foldl1 (+) (strToDouble month str)



main = do
    a <- getLine
    b <- getLine
    let result = logMes a b
    print result