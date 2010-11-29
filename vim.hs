import Prelude hiding (Left, Right)

import System.Environment
import System.IO
import Char
--(isAlphaNum)


import Position

data Mode      = Insert | Command

type Line     = String
type Lines    = [Line]
type Command  = String





runCommand :: String -> IO ()
runCommand = putStr

clearScreen :: String
clearScreen = escapeCode "[2J"

cursorToHome :: String
cursorToHome = cursorToPos (Position 1 1)

cursorToPos :: Position -> String
cursorToPos (Position x y) = escapeCode "[" ++ (show y) ++ ";" ++ (show x) ++ "f"

scroll :: Direction -> Int -> IO()
scroll _ 0       = return ()
scroll dir (n+1) = do runCommand (scrollCode dir)
                      scroll dir n

escapeCode :: String -> String
escapeCode = ("\ESC" ++)

scrollCode :: Direction -> String
scrollCode Up   = escapeCode "M"
scrollCode Down = escapeCode "D"
scrollCode _    = "\BEL"

beforeAndAfter :: [a] -> Int -> ([a], a, [a])
beforeAndAfter items n
  | length items >= n = (init start, last start, end)
                        where (start, end) = splitBefore n items

splitBefore :: Int -> [a] -> ([a],[a])
splitBefore 0 arr = ([],arr)
splitBefore n arr = splitAt n arr

insertCharacterInLine :: Line -> Position -> Char -> (Lines, Position)
insertCharacterInLine l (Position x y) '\n' = ([start, end], (Position 1 (y+1)))
                                              where start = fst splitLine
                                                    end   = snd splitLine
                                                    splitLine = splitAt x l
insertCharacterInLine l (Position x y) c    = ([start ++ [c] ++ end], (Position (x+1) y))
                                              where start = fst splitLine
                                                    end   = snd splitLine
                                                    splitLine = splitBefore x l


insertCharacterInDocument :: Lines -> Position -> Char -> (Lines, Position)
insertCharacterInDocument [] pos char = insertCharacterInDocument [""] pos char
insertCharacterInDocument lines (Position x y) char
  | (isControl char) && (char /= '\n') = (lines, (Position x y))
  | otherwise             = (bef ++ insertCharacterInDocument ++ aft, pos)
                            where (bef, cur, aft) = beforeAndAfter lines y
                                  modify      = insertCharacterInLine cur (Position x y) char
                                  insertCharacterInDocument = fst modify
                                  pos         = snd modify

getCh :: IO Char
getCh  = do hSetEcho stdin False
            c <- getChar
            hSetEcho stdin True
            return c
                                  
updateScreen :: Lines -> Position -> IO ()
updateScreen ls (Position x y) = do runCommand clearScreen 
                                    runCommand cursorToHome
                                    putStr $ unlines ls
                                    runCommand $ cursorToPos (Position x y)

insertMode :: Lines -> Position -> IO ()
insertMode ls cursorPos = do updateScreen ls cursorPos
                             input <- getCh
                             case input of
                               '\ESC'    -> commandMode ls cursorPos
                               otherwise -> do let (newLines, (Position newX newY)) = insertCharacterInDocument ls cursorPos input
                                               insertMode newLines (Position newX newY)

isCommandFinished :: String -> Bool
isCommandFinished ""   = False
isCommandFinished "dd" = True
isCommandFinished cmd  = elem (head cmd) "hjklioaA0$"


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

processCommand "0"  ls (Position x y) = (Command, ls, (Position 1 y))
processCommand "$"  ls (Position x y) = (Command, ls, newPos)
                               where currentLine = ls !! (y-1)
                                     newPos      = (Position (length currentLine) y)

processCommand "i"  ls pos = (Insert, ls, pos)
processCommand "o"  ls pos = (Insert, ls, setX (move pos Down) 1)
processCommand "a"  ls pos = (Insert, ls, move pos Right)
processCommand "A"  ls (Position x y) = (Insert, ls, newPos)
                                        where currentLine = ls !! (y-1)
                                              newPos      = (Position ((length currentLine)+1)  y)

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

