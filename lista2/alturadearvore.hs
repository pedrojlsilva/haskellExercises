data Tree t = Node t (Tree t) (Tree t) | Nilt deriving (Read)

alturaArvore :: Tree t -> Int
alturaArvore Nilt = 0
alturaArvore (Node t b c) = 1 + max (alturaArvore b) (alturaArvore c)
