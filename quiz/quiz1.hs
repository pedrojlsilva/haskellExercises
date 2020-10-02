
doubleMe xs = [x*2 | x <- xs]

mysteriousFunction :: [a]->[a]
mysteriousFunction x = take 5 x

fs = map
f1=(*2)
f2=(^2)

fsf1= fs f1
fsf2 = fs f2

func z = map (negate . abs) z