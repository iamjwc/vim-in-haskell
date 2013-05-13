
-- http://hpaste.org/annotate/57628



module Vim.IOUtil where

import UI.HSCurses.Curses as Curses (wclear, stdScr, mvWAddStr, refresh, scrSize, move, Pair(..), setBold, attr0, wAttrSet, wAttrGet, attrOn, attrBold, wAttrOn, attrBoldOn, beep)

import System.Console.Terminfo.PrettyPrint (ring, Bell(..))

import Vim.Position
import Vim.Mode

attrReverse :: Int
attrReverse = 262144
-- Also reverse? Curses.wAttrOn Curses.stdScr 65536

attrUnderline :: Int
attrUnderline = 131072

attrCrazyCharacters :: Int
attrCrazyCharacters = 4194304

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
