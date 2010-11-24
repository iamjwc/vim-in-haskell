import System.Environment
import System.IO
import Char
--(isAlphaNum)

data Direction = Up | Down | Left | Right

type Line     = String
type Lines    = [Line]
type Command  = String
type Mode     = String
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

seqn :: [IO a] -> IO ()
seqn []     = return ()
seqn (a:as) = do a
                 seqn as

wait :: Int -> IO ()
wait n = seqn [return () | _ <- [1..n]]

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

modifyLine :: Line -> Position -> Char -> (Lines, Position)
modifyLine l (x,y) '\n' = ([start, end], (1,y+1))
                          where start = fst splitLine
                                end   = snd splitLine
                                splitLine = splitAt x l
modifyLine l (x,y) c    = ([start ++ [c] ++ end], (x+1,y))
                          where start = fst splitLine
                                end   = snd splitLine
                                splitLine = splitBefore x l


modifyLines :: Lines -> Position -> Char -> (Lines, Position)
modifyLines [] pos char = modifyLines [""] pos char
modifyLines lines (x,y) char
  | (isControl char) && (char /= '\n') = (lines, (x,y))
--  | not (isAlphaNum char) = (lines, (x,y))
  | otherwise             = (bef ++ modifyLines ++ aft, pos)
                            where splitLines = beforeAndAfter lines y
                                  bef = beforeLines splitLines
                                  cur = currentLine splitLines
                                  aft = afterLines  splitLines
                                  modify      = modifyLine cur (x,y) char
                                  modifyLines = fst modify
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
                               otherwise -> do let (newLines, (newX, newY)) = modifyLines ls cursorPos input
                                               insertMode newLines (newX, newY)

isCommandFinished :: String -> Bool
isCommandFinished cmd = elem (head cmd) "hjkli"


getCommand :: String -> IO String
getCommand cmd
  | isCommandFinished cmd = return cmd
  | otherwise             = do input <- getCh
                               case input of
                                 '\ESC' -> return ""
                                 _      -> getCommand (cmd ++ [input])

processCommand :: String -> Lines -> Position -> IO ()
processCommand "h" ls (x,y) = commandMode ls ((max (x-1) 1), y)
processCommand "k" ls (x,y) = commandMode ls (x, (max (y-1) 1))
processCommand "l" ls (x,y) = commandMode ls (x+1, y)
processCommand "j" ls (x,y) = commandMode ls (x, y+1)
processCommand "i" ls (x,y) = insertMode ls (x,y)
processCommand o   ls (x,y) = commandMode ls (x,y)

commandMode :: Lines -> Position -> IO ()
commandMode ls pos = do updateScreen ls pos
                        cmd <- getCommand ""
                        processCommand cmd ls pos

 
-- | 'main' runs the main program
main :: IO ()
main = do runCommand clearScreen 
          runCommand cursorToHome
          hSetBuffering stdin NoBuffering
          hSetBuffering stdout NoBuffering
          insertMode [""] (1,1)

