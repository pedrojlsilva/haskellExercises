data Command = Forward Int | Backward Int | TurnLeft | TurnRight 
               deriving (Eq, Show, Read)

data Direction = North | South | West | East
                 deriving (Read, Show)

findDirection::Direction -> [Command] -> Direction
findDirection direction [] = direction
findDirection direction command
    | show direction == "North" && (head command) == TurnLeft  = findDirection West (tail command)
    | show direction == "North" && (head command) == TurnRight = findDirection East (tail command)
    | show direction == "South" && (head command) == TurnLeft  = findDirection East (tail command)
    | show direction == "South" && (head command) == TurnRight = findDirection West (tail command)
    | show direction == "West"  && (head command) == TurnLeft  = findDirection South (tail command)
    | show direction == "West"  && (head command) == TurnRight = findDirection North (tail command)
    | show direction == "East"  && (head command) == TurnLeft  = findDirection North (tail command)
    | show direction == "East"  && (head command) == TurnRight = findDirection South (tail command)
    | otherwise = findDirection direction (tail command)

faces :: Direction -> [Command] -> Direction
faces direction [] = direction 
faces direction command = findDirection direction command