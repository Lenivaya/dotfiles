module XMonad.Custom.Minimize where

import           Control.Monad
import           Data.Foldable
import           Data.Maybe
import           Graphics.X11.Types
import           XMonad
import           XMonad.Actions.GridSelect
import           XMonad.Actions.Minimize
import           XMonad.Core
import           XMonad.Custom.Prompt

selectMaximizeWindow :: X ()
selectMaximizeWindow = withMinimized selectMaximizeWindow'
 where
  selectMaximizeWindow' minimizedWindows = do
    minimizedWindowTitles <- mapM getWinTitle minimizedWindows
    selectedWin           <- gridselect gridMaximizeTheme
      $ zip minimizedWindowTitles minimizedWindows
    forM_ selectedWin maximizeWindow

getWinTitle :: Window -> X String
getWinTitle w = mconcat
  <$> sequence [runQuery title w, separator, runQuery appName w]
  where separator = pure " : "

gridMaximizeTheme :: GSConfig Window
gridMaximizeTheme =
  gridSelectTheme { gs_cellwidth = 350, gs_font = "xft:monospace:size=10" }
