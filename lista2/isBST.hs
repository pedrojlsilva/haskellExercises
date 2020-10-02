

data Tree t = Nilt | Node t (Tree t) (Tree t) deriving (Read)

isBSTaux :: (t -> Bool) -> Tree t -> Bool
isBSTaux _ Nilt = True
isBSTaux c (Node h l r) = c h && isBSTaux c l && isBSTaux c r



isBST :: Ord t => Tree t -> Bool
isBST Nilt = True
isBST (Node value left right) = isBSTaux (<=value) left && isBSTaux (>=value) right && isBST left && isBST right
