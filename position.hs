
module Position where

data Position  = Position Int Int
data Direction = Up | Down | Left | Right

move :: Position -> Direction -> Position
move (Position x y) Position.Left  = (Position (max (x-1) 1)  y)
move (Position x y) Up    = (Position x (max (y-1) 1))
move (Position x y) Position.Right = (Position (x+1) y)
move (Position x y) Down  = (Position x (y+1))

setX :: Position -> Int -> Position
setX (Position _ y) x  = (Position x y)

setY :: Position -> Int -> Position
setY (Position x _) y  = (Position x y)

getX :: Position -> Int
getX (Position x _) = x

getY :: Position -> Int
getY (Position _ y) = y

