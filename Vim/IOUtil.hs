
module IOUtil where

import System.IO
import UI.HSCurses.Curses as Curses

import Position
import VimMode

updateScreen :: Mode -> Lines -> Position -> IO ()
updateScreen mode ls (Position x y) = do Curses.wclear Curses.stdScr
                                         Curses.mvWAddStr Curses.stdScr 0 0 (unlines ls)
                                         statusBar mode (Position x y)
                                         case mode of
                                           ColonCommand _ -> return ()
                                           _              -> Curses.move y x
                                         Curses.refresh

statusBar :: Mode -> Position -> IO ()
statusBar Command (Position x y) = do (height,width) <- Curses.scrSize
                                      Curses.mvWAddStr Curses.stdScr (height-2) 0 ("x: " ++ (show x) ++ ", y: " ++ (show y))
                                      return ()

statusBar (ColonCommand cmd) (Position x y) = do (height,width) <- Curses.scrSize
                                                 Curses.mvWAddStr Curses.stdScr (height-2) 0 ("x: " ++ (show x) ++ ", y: " ++ (show y))
                                                 Curses.mvWAddStr Curses.stdScr (height-1) 0 cmd
                                                 Curses.move (height-1) (length cmd)
                                                 return ()

statusBar Insert  (Position x y) = do (height,width) <- Curses.scrSize
                                      Curses.mvWAddStr Curses.stdScr (height-2) 0 ("x: " ++ (show x) ++ ", y: " ++ (show y))
                                      Curses.mvWAddStr Curses.stdScr (height-1) 0 "-- INSERT --"
                                      return ()
