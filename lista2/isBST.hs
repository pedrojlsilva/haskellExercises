
-- https://stackoverflow.com/questions/48365022/determine-if-binary-tree-is-bst-haskell

data Tree t = Nilt | Node t (Tree t) (Tree t) deriving (Read)

isBST :: Ord t => Tree t -> Bool
isBST Nilt = True
isBST (Node h l r) = f (<=h) l && f (>=h) r && isBST l && isBST r
   where
     f _ Nilt = True
     f c (Node h l r) = c h && f c l && f c r