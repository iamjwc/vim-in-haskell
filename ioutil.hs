
module IOUtil where

import System.IO
import UI.HSCurses.Curses as Curses

import Position

updateScreen :: Lines -> Position -> IO ()
updateScreen ls (Position x y) = do Curses.wclear Curses.stdScr
                                    Curses.mvWAddStr Curses.stdScr 0 0 $ unlines ls
                                    Curses.move y x
                                    Curses.refresh
                           


runCommand :: String -> IO ()
runCommand = putStr

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
