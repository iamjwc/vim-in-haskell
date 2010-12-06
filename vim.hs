
-- Position includes its own Left and Right
import Prelude hiding (Left, Right, log)

import Control.Exception (bracket_)

import System.Environment
import System.IO
import Char
import Debug.Trace
--(isAlphaNum)

import UI.HSCurses.Curses as Curses


import Position

import IOUtil
import Util
import VimMode

data ColonCommandResponse = Quit | KeepGoing

type History = [(Lines, Position)]
data HistoryAction = Do | Undo | Ignore


insertCharacterInLine :: Line -> Position -> Char -> (Lines, Position)
insertCharacterInLine line pos '\n' = ([start, end], newPos)
                                      where (start, end) = splitAt (getX pos) line
                                            newPos       = Position.move (setX pos 0) Down
insertCharacterInLine line pos char = ([insertAt char (getX pos) line], Position.move pos Right)


insertCharacterInDocument :: Lines -> Position -> Char -> (Lines, Position)
insertCharacterInDocument [] pos char = insertCharacterInDocument [""] pos char
insertCharacterInDocument lines pos char
  | (isControl char) && (char /= '\n') = (lines, newPos)
  | otherwise                          = (bef ++ newLine ++ aft, newPos)
                                         where (bef, cur, aft)   = beforeAndAfter lines (getY pos)
                                               (newLine, newPos) = insertCharacterInLine cur pos char


isCommandFinished :: String -> Bool
isCommandFinished ""       = False
isCommandFinished "dd"     = True
isCommandFinished "gg"     = True
isCommandFinished ('r':c:[]) = True
isCommandFinished cmd      = elem (head cmd) "uhjklioaA0$xDG:"


getCommand :: String -> IO String
getCommand cmd
  | isCommandFinished cmd = do return cmd
  | otherwise             = do input <- Curses.getCh
                               case input of
                                 KeyChar '\ESC' -> return ""
                                 KeyChar c      -> getCommand (cmd ++ [c])

deleteLine :: Lines -> Position -> (Lines, Position)
deleteLine ls (Position x y)
  -- If trying to delete the last line
  | y == (length ls)-1 = (init ls, (Position x (y-1)))
  -- first line
  | y == 0             = (tail ls, (Position x y))
  | otherwise          = (init before ++ after, (Position x y))
                         where (before,after) = splitBefore (y+1) ls


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
-- Redo character = '\DC2'

processCommand ":"  ls pos = (ColonCommand ":", Ignore, (ls, pos))

processCommand "gg" ls pos = (Command, Ignore, (ls, setY pos 0))
processCommand "G"  ls pos = (Command, Ignore, (ls, setY pos (length ls)))
processCommand "h"  ls pos = (Command, Ignore, (ls, Position.move pos Left))
processCommand "k"  ls pos = (Command, Ignore, (ls, Position.move pos Up))
processCommand "l"  ls pos = (Command, Ignore, (ls, Position.move pos Right))
processCommand "j"  ls pos = (Command, Ignore, (ls, Position.move pos Down))

processCommand "dd" ls pos = (Command, Do, deleteLine ls pos)
processCommand "x"  ls pos = (Command, Do, deleteCharacter ls pos)
processCommand "D"  ls pos = (Command, Do, deleteToEndOfLine ls pos)

processCommand "0"  ls (Position x y) = (Command, Ignore, (ls, (Position 0 y)))
processCommand "$"  ls (Position x y) = (Command, Ignore, (ls, newPos))
                               where currentLine = ls !! y
                                     newPos      = (Position (length currentLine) y)

processCommand ('r':c:[]) ls pos = (Command, Do, replaceCharacter ls pos c)


processCommand "i"  ls pos = (Insert, Do, (ls, pos))
processCommand "a"  ls pos = (Insert, Do, (ls, Position.move pos Right))
processCommand "A"  ls (Position x y) = (Insert, Do, (ls, newPos))
                                        where currentLine = ls !! y
                                              newPos      = (Position ((length currentLine)+1)  y)
processCommand "o"  ls (Position _ y) = (Insert, Do, (newLs, newPos))
                                        where currentLine     = ls !! y
                                              (newLs, newPos) = insertCharacterInDocument ls (Position (length currentLine) y) '\n'

processCommand o    ls pos   = (Command, Do, (ls, pos))

determineNewPath :: String -> Lines -> Position -> History -> (Mode, Lines, Position, History)
determineNewPath cmd ls pos history = (mode, newLs, newPos, newHistory)
                                      where (mode, historyAction, (returnedLs, returnedPos)) = processCommand cmd ls pos
                                            -- Determine the new lines and position
                                            (newLs, newPos) = case (historyAction, history) of
                                              (Undo, []) -> (ls, pos)
                                              (Undo, _)  -> head $ tail history
                                              _          -> (returnedLs, moveToValidPosition mode returnedLs returnedPos)

                                            -- Determine the new history
                                            newHistory = case (historyAction, history) of
                                              (Undo, [])  -> []
                                              (Undo, _)   -> (newLs, newPos) : (tail $ tail history)
                                              (Do, _)     -> (returnedLs, returnedPos):history
                                              (Ignore, _) -> history

log :: String -> IO ()
log s = do f <- openFile "debug.log" AppendMode
           hPutStrLn f s
           hClose f

backspace :: Lines -> Position -> (Lines, Position)
backspace ls (Position 0 y) = (ls, Position 0 y)
backspace ls pos            = (newLs, newPos)
                              where (newLs, _) = deleteCharacter ls (Position.move pos Left) -- Deletes the character to the left of the position
                                    newPos     = (Position.move pos Left)

saveFile :: String -> Lines -> IO (Lines)
saveFile name ls = do writeFile name $ unlines ls
                      return ls

loadFile :: String -> IO (Lines)
loadFile name = do s <- readFile name
                   return $ lines s

processColonCommand :: String -> Lines -> Position -> IO (ColonCommandResponse, Lines)
processColonCommand (':':cmd) ls pos = processColonCommand cmd ls pos

processColonCommand ('w':' ':name) ls pos = do newLs <- saveFile name ls
                                               return (KeepGoing, newLs)
processColonCommand ('e':' ':name) ls pos = do newLs <- loadFile name
                                               return (KeepGoing, newLs)
processColonCommand "q"            ls pos = return (Quit, [])

colonCommandMode :: String -> Lines -> Position -> History -> IO ()
colonCommandMode cmd ls pos history = do updateScreen (ColonCommand cmd) ls pos
                                         input <- Curses.getCh
                                         case input of
                                           KeyChar '\ESC' -> commandMode ls pos history
                                           KeyChar '\n'   -> do (resp, newLs) <- processColonCommand cmd ls pos
                                                                case resp of
                                                                  Quit -> return ()
                                                                  _    -> if ls == newLs
                                                                            then commandMode ls pos history
                                                                            else commandMode newLs (Position 0 0) []
                                           KeyChar '\DEL' -> colonCommandMode (init cmd)   ls pos history
                                           KeyChar c      -> colonCommandMode (cmd ++ [c]) ls pos history

commandMode :: Lines -> Position -> History -> IO ()
commandMode [] _   history = commandMode [""] (Position 0 0) history
commandMode ls pos []      = commandMode ls pos [(ls,pos)]
commandMode ls pos history = do updateScreen Command ls pos
                                log $ show $ map (fst) history
                                cmd <- getCommand ""
                                case determineNewPath cmd ls pos history of
                                  (Insert,           newLs, newPos, newHistory) -> insertMode newLs newPos newHistory
                                  (Command,          newLs, newPos, newHistory) -> commandMode newLs newPos newHistory
                                  (ColonCommand cmd, newLs, newPos, newHistory) -> colonCommandMode cmd newLs newPos newHistory

insertMode :: Lines -> Position -> History -> IO ()
insertMode ls cursorPos history = do updateScreen Insert ls cursorPos
                                     log $ show $ map (fst) history
                                     input <- Curses.getCh
                                     case input of
                                       KeyChar '\ESC' -> commandMode ls (Position.move cursorPos Left) ((ls,cursorPos):history)
                                       KeyChar '\DEL' -> do let (newLines, newPos) = backspace ls cursorPos
                                                            insertMode newLines newPos history
                                       KeyChar c      -> do log [c]
                                                            log $ show (fromEnum c, c)
                                                            log $ show ls
                                                            log $ "Position x:" ++ (show (getX cursorPos)) ++ " y: " ++ (show (getY cursorPos))
                                                            let (newLines, newPos) = insertCharacterInDocument ls cursorPos c
                                                            insertMode newLines newPos history


start :: IO ()
start  = do Curses.initScr
            -- Curses.keypad Curses.stdScr True
            -- Curses.nl False
            -- Curses.cBreak True
            Curses.echo False
            return ()

end :: IO ()
end  = do Curses.endWin
          return ()

-- main :: IO ()
-- main  = do let line  = ""
--            let lines = [line]
--            let (newls:[],newp) = insertCharacterInDocument lines (Position 0 0) 'f'
--            log newls
--            return ()

-- | 'main' runs the main program
main :: IO ()
main = bracket_ start end (commandMode [] (Position 0 0) [])


