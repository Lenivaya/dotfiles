module XMonad.Custom.Actions.Minimize where

import Control.Monad
import Data.Foldable
import Data.Functor
import Data.Maybe
import Graphics.X11.Types
import XMonad
import XMonad.Actions.GridSelect
import XMonad.Actions.Minimize
import XMonad.Core
import XMonad.Custom.Prompt
import XMonad.Prompt

getWinTitle :: Window -> X String
getWinTitle w =
  mconcat
    <$> sequence [runQuery title w, separator, runQuery appName w]
  where
    separator = pure " : "

gridMaximizeTheme :: GSConfig Window
gridMaximizeTheme =
  gridSelectTheme {gs_cellwidth = 400, gs_font = "xft:monospace:size=10"}

selectMaximizeWindowGrid :: X ()
selectMaximizeWindowGrid = withMinimized $ \minimizedWindows ->
  mapM getWinTitle minimizedWindows
    >>= gridselect gridMaximizeTheme . flip zip minimizedWindows
    >>= mapM_ maximizeWindow

data MaximizeWindowPrompt = MaximizeWindowPrompt

instance XPrompt MaximizeWindowPrompt where
  showXPrompt _ = "Restore window: "

selectMaximizeWindowPrompt :: XPConfig -> X ()
selectMaximizeWindowPrompt conf = withMinimized $ \minimizedWindows ->
  case minimizedWindows of
    [] -> return ()
    _ ->
      mapM getWinTitle minimizedWindows >>= \windowTitles ->
        mkXPrompt
          MaximizeWindowPrompt
          conf
          (listCompFunc conf windowTitles)
          (go minimizedWindows windowTitles)
  where
    go minimizedWindows windowTitles selected =
      forM_
        (lookup selected $ zip windowTitles minimizedWindows)
        maximizeWindow
