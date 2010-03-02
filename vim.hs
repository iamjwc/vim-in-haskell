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
cursorToHome = escapeCode "[1;1f"

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
modifyLine l (x,y) '\n' = ([start, end], (0,y+1))
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
insertMode ls cursorPos = do input <- getCh
                             case input of
                               '\ESC'    -> commandMode ls cursorPos
                               otherwise -> do let (newLines, (newX, newY)) = modifyLines ls cursorPos input
                                               updateScreen ls (newX, newY)
                                               insertMode newLines (newX, newY)

commandMode :: Lines -> Position -> IO ()
commandMode ls (x,y) = do input <- getCh
                          updateScreen ls (x, y)
                          case input of
                            'l' -> commandMode ls ((max (x-1) 0), y)
                            'k' -> commandMode ls (x, (max (y-1) 0))
                            'h' -> commandMode ls (x+1, y)
                            'j' -> commandMode ls (x, y+1)
                            'i' -> insertMode ls (x,y)
                            o   -> commandMode ls (x,y)

 
-- | 'main' runs the main program
main :: IO ()
main = do runCommand clearScreen 
          --runCommand $ escapeCode "[6n"
          runCommand cursorToHome
          --runCommand $ escapeCode "[6n"
          hSetBuffering stdin NoBuffering --runCommand clearScreen
          hSetBuffering stdout NoBuffering --runCommand clearScreen
          insertMode [""] (1,1)
          --putStr "hey\nyou\nblah"
          --a <- wait 50000000
          --print $ show a
          --putStr "\ESCM"
