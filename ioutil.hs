
module IOUtil where

import System.IO

import Position

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

getCh :: IO Char
getCh  = do hSetEcho stdin False
            c <- getChar
            hSetEcho stdin True
            return c
