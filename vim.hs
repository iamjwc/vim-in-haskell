
-- Position includes its own Left and Right
import Prelude hiding (Left, Right)

import System.Environment
import System.IO
import Char
--(isAlphaNum)


import Position
import IOUtil
import Util
import Document

data Mode      = Insert | Command




insertCharacterInLine :: Line -> Position -> Char -> (Lines, Position)
insertCharacterInLine line pos '\n' = ([start, end], newPos)
                                      where (start, end) = splitAt (getX pos) line
                                            newPos       = move (setX pos 1) Down
insertCharacterInLine line pos char = ([insertBefore char (getX pos) line], move pos Right)


insertCharacterInDocument :: Lines -> Position -> Char -> (Lines, Position)
insertCharacterInDocument [] pos char = insertCharacterInDocument [""] pos char
insertCharacterInDocument lines pos char
  | (isControl char) && (char /= '\n') = (lines, newPos)
  | otherwise                          = (bef ++ newLine ++ aft, newPos)
                                         where (bef, cur, aft)   = beforeAndAfter lines (getY pos)
                                               (newLine, newPos) = insertCharacterInLine cur pos char


updateScreen :: Lines -> Position -> IO ()
updateScreen ls pos = do runCommand clearScreen 
                         runCommand cursorToHome
                         putStr $ unlines ls
                         runCommand $ cursorToPos pos

insertMode :: Lines -> Position -> IO ()
insertMode ls cursorPos = do updateScreen ls cursorPos
                             input <- getCh
                             case input of
                               '\ESC'    -> commandMode ls (move cursorPos Left)
                               otherwise -> do let (newLines, newPos) = insertCharacterInDocument ls cursorPos input
                                               insertMode newLines newPos

isCommandFinished :: String -> Bool
isCommandFinished ""   = False
isCommandFinished "dd" = True
isCommandFinished cmd  = elem (head cmd) "hjklioaA0$xD"


getCommand :: String -> IO String
getCommand cmd
  | isCommandFinished cmd = do return cmd
  | otherwise             = do input <- getCh
                               case input of
                                 '\ESC' -> return ""
                                 _      -> getCommand (cmd ++ [input])

deleteLine :: Lines -> Position -> (Lines, Position)
deleteLine ls (Position x y)
  -- If there are no lines
  | (length ls) == 0 = (ls,(Position x y))
  -- If trying to delete the last line
  | y == (length ls)  = (init ls, (Position x (y-1)))
  -- first line
  | y == 0           = (tail ls, (Position x y))
  | otherwise        = (init before ++ after, (Position x y))
                       where (before,after) = splitBefore y ls




processCommand :: String -> Lines -> Position -> (Mode, Lines, Position)
processCommand "dd" ls pos   = (Command, newLs, newPos)
                               where (newLs, newPos) = deleteLine ls pos
processCommand "h"  ls pos = (Command, ls, move pos Left)
processCommand "k"  ls pos = (Command, ls, move pos Up)
processCommand "l"  ls pos = (Command, ls, move pos Right)
processCommand "j"  ls pos = (Command, ls, move pos Down)

processCommand "x"  ls pos = (Command, (startLs ++ [startL ++ endL] ++ endLs), newPos)
                             where (Position x y) = pos
                                   (startLs, currentL, endLs) = beforeAndAfter ls y
                                   (startL, _, endL) = beforeAndAfter currentL x
                                   newPos = case endL of
                                     [] -> setX pos (length startL)
                                     _  -> pos

processCommand "D"  ls pos = (Command, (startLs ++ [startL] ++ endLs), newPos)
                             where (Position x y) = pos
                                   (startLs, currentL, endLs) = beforeAndAfter ls y
                                   (startL, _, _) = beforeAndAfter currentL x
                                   newPos = setX pos (length startL)

processCommand "0"  ls (Position x y) = (Command, ls, (Position 1 y))
processCommand "$"  ls (Position x y) = (Command, ls, newPos)
                               where currentLine = ls !! (y-1)
                                     newPos      = (Position (length currentLine) y)

processCommand "i"  ls pos = (Insert, ls, pos)
processCommand "a"  ls pos = (Insert, ls, move pos Right)
processCommand "A"  ls (Position x y) = (Insert, ls, newPos)
                                        where currentLine = ls !! (y-1)
                                              newPos      = (Position ((length currentLine)+1)  y)
processCommand "o"  ls (Position _ y) = (Insert, newLs, newPos)
                                        where currentLine     = ls !! (y-1)
                                              (newLs, newPos) = insertCharacterInDocument ls (Position (length currentLine) y) '\n'

processCommand o    ls pos   = (Command, ls, pos)

-- lineAtCurrentPos :: Lines -> Position -> Line
-- lineAtCurrentPos

commandMode :: Lines -> Position -> IO ()
commandMode ls pos = do updateScreen ls pos
                        cmd <- getCommand ""
                        case processCommand cmd ls pos of
                          (Insert, newLs, newPos)  -> insertMode newLs newPos
                          (Command, newLs, newPos) -> commandMode newLs newPos


-- | 'main' runs the main program
main :: IO ()
main = do runCommand clearScreen 
          runCommand cursorToHome
          hSetBuffering stdin NoBuffering
          hSetBuffering stdout NoBuffering
          commandMode [""] (Position 1 1)

