data Tree t = Nilt |
               Node t (Tree t) (Tree t)
               deriving (Read, Show)

insert :: Ord a => Tree a -> a -> Tree a
insert Nilt x = Node x Nilt Nilt
insert (Node v t1 t2) x 
    | v == x = Node v t1 t2
    | v  < x = Node v t1 (insert t2 x)
    | v  > x = Node v (insert t1 x) t2

insertList :: Ord a =>  Tree a->[a]->Tree a
insertList tree [ ] = tree
insertList tree (x:xs) = insertList (insert tree x) xs -- chama de novo insertAux mas antes resolve insert

main = do
       a <- getLine
       b <- getLine
       let result = insertList (read a::Tree Int) (read b)
       print result



