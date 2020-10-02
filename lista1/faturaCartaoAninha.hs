
logMes :: String -> String -> Double
logMes str mes = l' (separa str) mes 
  where
    l' str mes = foldl1 (+) n 
      where n = [x4 | (x1, x2, x3, x4) <- str, x2 == mes]

separa = separa2 . separa1

separa2 :: [String] -> [(String, String, String, Double)]
separa2 [] = []
separa2 (x:(y:(z:zs))) = (x1, x2, y, x3) : (separa2 zs)
  where
    (x1, x2) = takeW (/= ' ') x 
    x3 = (read z)::Double
    


separa1 :: String -> [String]
separa1 [] = []
separa1 str = r1 : separa1 r2
  where (r1, r2) = takeW (/= ';') str


takeW :: (Char -> Bool) -> String -> (String, String)
takeW cmp str = t' cmp str ([],[])
  where 
    t' cmp [] res = res
    t' cmp (x:xs) (r1, r2)
      | (cmp x) = t' cmp xs (r1 ++ [x], r2)
      | otherwise = (r1, xs)