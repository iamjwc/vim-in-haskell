import System.Environment
import System.IO
import Char
--(isAlphaNum)

data Direction = Up | Down | Left | Right
data Mode      = Insert | Command

type Line     = String
type Lines    = [Line]
type Command  = String
type Position = (Int, Int)

runCommand :: String -> IO ()
runCommand = putStr

clearScreen :: String
clearScreen = escapeCode "[2J"

cursorToHome :: String
cursorToHome = cursorToPos 1 1

cursorToPos :: Int -> Int -> String
cursorToPos x y = escapeCode "[" ++ (show y) ++ ";" ++ (show x) ++ "f"

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

beforeLines :: (Lines, Line, Lines) -> Lines
beforeLines (b, c, a) = b

afterLines :: (Lines, Line, Lines) -> Lines
afterLines (b, c, a) = a

currentLine :: (Lines, Line, Lines) -> Line
currentLine (b, c, a) = c

beforeAndAfter :: [a] -> Int -> ([a], a, [a])
beforeAndAfter items n
  | length items >= n = (before, current, after)
                        where before     = init $ start
                              after      = end
                              current    = last $ start
                              (start, end) = splitBefore n items

splitBefore :: Int -> [a] -> ([a],[a])
--splitBefore n arr
--  | length arr < 2 = (arr, [])
--  | otherwise      = (start, end)
--                     where splitArr = splitAt n arr
--                           start    = init $ fst splitArr
--                           end      = [(last $ fst splitArr)] ++ snd splitArr
splitBefore 0 arr = ([],arr)
splitBefore n arr = splitAt n arr

insertCharacterInLine :: Line -> Position -> Char -> (Lines, Position)
insertCharacterInLine l (x,y) '\n' = ([start, end], (1,y+1))
                          where start = fst splitLine
                                end   = snd splitLine
                                splitLine = splitAt x l
insertCharacterInLine l (x,y) c    = ([start ++ [c] ++ end], (x+1,y))
                          where start = fst splitLine
                                end   = snd splitLine
                                splitLine = splitBefore x l


insertCharacterInDocument :: Lines -> Position -> Char -> (Lines, Position)
insertCharacterInDocument [] pos char = insertCharacterInDocument [""] pos char
insertCharacterInDocument lines (x,y) char
  | (isControl char) && (char /= '\n') = (lines, (x,y))
--  | not (isAlphaNum char) = (lines, (x,y))
  | otherwise             = (bef ++ insertCharacterInDocument ++ aft, pos)
                            where (bef, cur, aft) = beforeAndAfter lines y
                                  modify      = insertCharacterInLine cur (x,y) char
                                  insertCharacterInDocument = fst modify
                                  pos         = snd modify

getCh :: IO Char
getCh  = do hSetEcho stdin False
            c <- getChar
            hSetEcho stdin True
            return c
                                  
updateScreen :: Lines -> Position -> IO ()
updateScreen ls (x,y) = do runCommand clearScreen 
                           runCommand cursorToHome
                           putStr $ unlines ls
                           runCommand $ cursorToPos x y

insertMode :: Lines -> Position -> IO ()
insertMode ls cursorPos = do updateScreen ls cursorPos
                             input <- getCh
                             case input of
                               '\ESC'    -> commandMode ls cursorPos
                               otherwise -> do let (newLines, (newX, newY)) = insertCharacterInDocument ls cursorPos input
                                               insertMode newLines (newX, newY)

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
deleteLine ls (x,y)
  -- If there are no lines
  | (length ls) == 0 = (ls,(x,y))
  -- If trying to delete the last line
  | y == (length ls)  = (init ls, (x,y-1))
  -- first line
  | y == 0           = (tail ls, (x,y))
  | otherwise        = (init before ++ after, (x,y))
                       where (before,after) = splitBefore y ls




processCommand :: String -> Lines -> Position -> (Mode, Lines, Position)
processCommand "dd" ls pos   = (Command, newLs, newPos)
                               where (newLs, newPos) = deleteLine ls pos
processCommand "h"  ls (x,y) = (Command, ls, ((max (x-1) 1), y))
processCommand "k"  ls (x,y) = (Command, ls, (x, (max (y-1) 1)))
processCommand "l"  ls (x,y) = (Command, ls, (x+1, y))
processCommand "j"  ls (x,y) = (Command, ls, (x, y+1))

processCommand "0"  ls (x,y) = (Command, ls, (1,y))
processCommand "$"  ls (x,y) = (Command, ls, newPos)
                               where currentLine = ls !! (y-1)
                                     newPos      = (length currentLine, y)

processCommand "i"  ls pos   = (Insert, ls, pos)
processCommand "o"  ls (x,y) = (Insert, ls, (x,y+1))
processCommand "a"  ls (x,y) = (Insert, ls, (x+1,y))
processCommand "A"  ls (x,y) = (Insert, ls, newPos)
                               where currentLine = ls !! (y-1)
                                     newPos      = ((length currentLine)+1, y)

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
          commandMode [""] (1,1)

