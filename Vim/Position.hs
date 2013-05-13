
module Vim.Position where

import Prelude hiding (Left, Right, log)
import Vim.Mode

type Line     = String
type Lines    = [Line]

data Position  = Position Int Int
data Direction = Up | Down | Left | Right

move :: Position -> Direction -> Position
move (Position x y) Left  = Position (max (x-1) 0)  y
move (Position x y) Up    = Position x (max (y-1) 0)
move (Position x y) Right = Position (x+1) y
move (Position x y) Down  = Position x (y+1)

setX :: Position -> Int -> Position
setX (Position _ y) x  = Position x y

setY :: Position -> Int -> Position
setY (Position x _) y  = Position x y

getX :: Position -> Int
getX (Position x _) = x

getY :: Position -> Int
getY (Position _ y) = y

isValid :: Mode -> Lines -> Position -> Bool
isValid mode ls (Position x y) = x == otherX && y == otherY
                                 where Position otherX otherY = moveToValidPosition mode ls $ Position x y

moveToValidPosition :: Mode -> Lines -> Position -> Position
-- Insert mode allows the position
-- after the last character in a
-- line to be valid. This way you
-- can append to the end of a line
moveToValidPosition Insert ls (Position x y) = moveToValidPosition Command newLs $ Position x y
                                               where newLs = map (++" ") ls
moveToValidPosition mode ls (Position x y)
  | y >= lenDoc  = moveToValidPosition mode ls $ Position x (lenDoc-1)
  | x >= lenLine = moveToValidPosition mode ls $ Position (lenLine-1) y
  | otherwise    = Position (max x 0) (max y 0)
                   where lenLine = length (ls !! y)
                         lenDoc  = length ls

