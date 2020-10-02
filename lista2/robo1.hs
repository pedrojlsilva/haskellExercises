data Command = Forward Int | Backward Int | TurnLeft | TurnRight 
               deriving (Eq, Show, Read)

data Direction = North | South | West | East
                 deriving (Read, Show)

findDirection::Direction -> [Command] -> (Int, Int) -> (Int,Int)
findDirection direction [] point = point
findDirection North (TurnLeft:as) point = findDirection West (as)  point
findDirection North (TurnRight:as) point = findDirection East (as)  point
findDirection South (TurnLeft:as) point  = findDirection East (as)  point
findDirection South (TurnRight:as) point = findDirection West (as)  point
findDirection West  (TurnLeft:as) point = findDirection South (as) point
findDirection West  (TurnRight:as) point = findDirection North (as) point
findDirection East  (TurnLeft:as) point = findDirection North (as) point
findDirection East  (TurnRight:as) point = findDirection South (as) point

findDirection North (Forward n:as) point = findDirection North (as)  ((fst point), (snd point)+n)
findDirection North (Backward n:as) point = findDirection North (as)  ((fst point), (snd point)-n)
findDirection South (Forward n:as) point  = findDirection South (as)  ((fst point), (snd point)-n)
findDirection South (Backward n:as) point = findDirection South (as)  ((fst point), (snd point)+n)
findDirection West  (Forward n:as) point = findDirection West (as) ((fst point)-n, (snd point))
findDirection West  (Backward n:as) point = findDirection West (as) ((fst point)+n, (snd point))
findDirection East  (Forward n:as) point = findDirection East (as) ((fst point)+n, (snd point))
findDirection East  (Backward n:as) point = findDirection East (as) ((fst point)-n, (snd point))





destination :: (Int,Int) -> [Command] -> (Int,Int)
destination point [] = point
destination point command = findDirection North command point


main = do
       a <- getLine
       b <- getLine
       let result = destination (read a) (read b)
       print result