data Ops = SUM | MUL | SUB
           deriving (Read)

data IntTree = Nilt Int |
               Node Ops IntTree IntTree
               deriving (Read)

isSum::Ops->Bool
isSum SUM = True
isSum SUB = False
isSum MUL = False

isSub::Ops->Bool
isSub SUM = False
isSub SUB = True
isSub MUL = False

isMul::Ops->Bool
isMul SUM = False
isMul SUB = False
isMul MUL = True


evalTree :: IntTree -> Int
evalTree (Nilt number) = number
evalTree (Node op a b)
    | isSum op = evalTree a + evalTree b
    | isSub op = evalTree a - evalTree b
    | isMul op = evalTree a * evalTree b