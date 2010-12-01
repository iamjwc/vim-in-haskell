
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

type History = [(Lines, Position)]
data HistoryAction = Do | Undo | Ignore




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

insertMode :: Lines -> Position -> History -> IO ()
insertMode ls cursorPos history = do updateScreen ls cursorPos
                                     input <- getCh
                                     case input of
                                       '\ESC'    -> commandMode ls (move cursorPos Left) history
                                       '\b'      -> do let (newLines, newPos) = deleteCharacter ls (move cursorPos Left)
                                                       insertMode [""] newPos history
                                       otherwise -> do let (newLines, newPos) = insertCharacterInDocument ls cursorPos input
                                                       insertMode newLines newPos history

isCommandFinished :: String -> Bool
isCommandFinished ""       = False
isCommandFinished "dd"     = True
isCommandFinished ('r':c:[]) = True
isCommandFinished cmd      = elem (head cmd) "uhjklioaA0$xD"


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


deleteToEndOfLine :: Lines -> Position -> (Lines, Position)
deleteToEndOfLine ls (Position x y) = ((startLs ++ [startL] ++ endLs), newPos)
                           where (startLs, currentL, endLs) = beforeAndAfter ls y
                                 (startL, _, _) = beforeAndAfter currentL x
                                 newPos = Position (length startL) y

deleteCharacter :: Lines -> Position -> (Lines, Position)
deleteCharacter ls (Position x y) = ((startLs ++ [startL ++ endL] ++ endLs), newPos)
                             where (startLs, currentL, endLs) = beforeAndAfter ls y
                                   (startL, _, endL) = beforeAndAfter currentL x
                                   newPos = case endL of
                                     [] -> Position (length startL) y
                                     _  -> Position x y

replaceCharacter :: Lines -> Position -> Char -> (Lines, Position)
replaceCharacter ls (Position x y) c = ((startLs ++ [startL ++ [c] ++ endL] ++ endLs), (Position x y))
                                       where (startLs, currentL, endLs) = beforeAndAfter ls y
                                             (startL, _, endL) = beforeAndAfter currentL x


processCommand :: String -> Lines -> Position -> (Mode, HistoryAction, (Lines, Position))
processCommand "u"  ls pos = (Command, Undo, (ls, pos))

processCommand "h"  ls pos = (Command, Ignore, (ls, move pos Left))
processCommand "k"  ls pos = (Command, Ignore, (ls, move pos Up))
processCommand "l"  ls pos = (Command, Ignore, (ls, move pos Right))
processCommand "j"  ls pos = (Command, Ignore, (ls, move pos Down))

processCommand "dd" ls pos = (Command, Do, deleteLine ls pos)
processCommand "x"  ls pos = (Command, Do, deleteCharacter ls pos)
processCommand "D"  ls pos = (Command, Do, deleteToEndOfLine ls pos)

processCommand "0"  ls (Position x y) = (Command, Ignore, (ls, (Position 1 y)))
processCommand "$"  ls (Position x y) = (Command, Ignore, (ls, newPos))
                               where currentLine = ls !! (y-1)
                                     newPos      = (Position (length currentLine) y)

processCommand ('r':c:[]) ls pos = (Command, Do, replaceCharacter ls pos c)

processCommand "i"  ls pos = (Insert, Do, (ls, pos))
processCommand "a"  ls pos = (Insert, Do, (ls, move pos Right))
processCommand "A"  ls (Position x y) = (Insert, Do, (ls, newPos))
                                        where currentLine = ls !! (y-1)
                                              newPos      = (Position ((length currentLine)+1)  y)
processCommand "o"  ls (Position _ y) = (Insert, Do, (newLs, newPos))
                                        where currentLine     = ls !! (y-1)
                                              (newLs, newPos) = insertCharacterInDocument ls (Position (length currentLine) y) '\n'

processCommand o    ls pos   = (Command, Do, (ls, pos))

-- lineAtCurrentPos :: Lines -> Position -> Line
-- lineAtCurrentPos

determineNewPath :: String -> Lines -> Position -> History -> (Mode, Lines, Position, History)
determineNewPath cmd ls pos history = (mode, newLs, newPos, newHistory)
                                      where (mode, historyAction, (returnedLs, returnedPos)) = processCommand cmd ls pos
                                            -- Determine the new lines and position
                                            (newLs, newPos) = case (historyAction, history) of
                                              (Undo, []) -> (ls, pos)
                                              (Undo, _)  -> head history
                                              _          -> (returnedLs, returnedPos)

                                            -- Determine the new history
                                            newHistory = case (historyAction, history) of
                                              (Undo, [])  -> []
                                              (Undo, _)   -> tail history
                                              (Do, _)     -> (returnedLs, returnedPos):history
                                              (Ignore, _) -> history

commandMode :: Lines -> Position -> History -> IO ()
commandMode ls pos history = do updateScreen ls pos
                                cmd <- getCommand ""
                                case determineNewPath cmd ls pos history of
                                  (Insert,  newLs, newPos, newHistory) -> insertMode newLs newPos newHistory
                                  (Command, newLs, newPos, newHistory) -> commandMode newLs newPos newHistory


-- | 'main' runs the main program
main :: IO ()
main = do runCommand clearScreen 
          runCommand cursorToHome
          hSetBuffering stdin NoBuffering
          hSetBuffering stdout NoBuffering
          commandMode [""] (Position 1 1) []

