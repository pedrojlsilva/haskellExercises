qsGrande :: [Int] -> [Int]
qsGrande [] =[]
qsGrande (c:cs) = qsGrande (menores c cs) ++ [c] ++ qsGrande (maiores c cs)


menores ::  Int -> [Int] -> [Int]
menores pivo [] = []
menores pivo (a:as)
    | pivo >= a = a : menores pivo as
    | otherwise = menores pivo as

maiores ::  Int -> [Int] -> [Int]
maiores pivo [] = []
maiores pivo (a:as)
    | pivo < a = a : maiores pivo as
    | otherwise = maiores pivo as

qsMedio ::  [Int] -> [Int]
qsMedio [] =[]
qsMedio (c:cs) = qsMedio (compara c cs (>=)) ++ [c] ++ qsMedio (compara c cs (<))

compara :: Int -> [Int] -> (Int -> Int -> Bool) -> [Int]
compara pivo [] cmp = []
compara pivo (a:as) cmp
    | cmp pivo a = a : compara pivo as cmp
    | otherwise = compara pivo as cmp



qsEsperto [] = []
qsEsperto (pivo:cauda) = 
    let (menores, maiores) = particionar pivo cauda ([],[])
    in qsEsperto menores ++ [pivo] ++ qsEsperto maiores


particionar :: Int -> [Int] -> ([Int], [Int]) -> ([Int], [Int])
particionar _ [] resultado = resultado
particionar pivo (a:as) (menores,maiores)
    | a<=pivo   = particionar pivo as (a:menores, maiores)
    | a> pivo   = particionar pivo as (menores, a:maiores)

qs ::  [Int] -> [Int] --com compreensão de lista
qs [] = []
qs(pivo:cs) = qs[x|x<-cs, x<=pivo] ++ [pivo] ++ qs[y|y<-cs, y>pivo]


