{-# LANGUAGE ImportQualifiedPost #-}

module XMonad.Custom.Utils.Loggers where

import Control.Monad
import Data.List (intersect)
import XMonad
import XMonad.Actions.Minimize
import XMonad.Custom.Theme
import XMonad.Hooks.StatusBar.PP
import XMonad.StackSet qualified as W
import XMonad.Util.ExtensibleState qualified as UXS
import XMonad.Util.Loggers
import XMonad.Util.Minimize

{-| A logger that displays the count of visible and minimized windows in the current workspace.
  The format is:
  * "[N]" when there are no minimized windows (N = total windows)
  * "[V] (H)" when there are minimized windows where:
    - V = visible windows count
    - H = hidden/minimized windows count

  Colors:
  * Window counts are displayed in white2
  * Hidden window count is displayed in red2
-}
windowsLogger :: Logger
windowsLogger = do
  windows <- gets $ W.integrate' . W.stack . W.workspace . W.current . windowset
  hiddenWindows <- UXS.gets minimizedStack

  let totalCount = length windows
      hiddenCount = length $ hiddenWindows `intersect` windows
      visibleCount = totalCount - hiddenCount
      isThereAnyWindow = totalCount > 0

      formatCount = xmobarColor white2 "" . wrap "[" "]" . show
      formatHidden = xmobarColor red2 "" . wrap "(" ")" . show

  ( if isThereAnyWindow
      then
        return $
          Just $
            if hiddenCount <= 0
              then formatCount totalCount
              else formatCount visibleCount ++ " " ++ formatHidden hiddenCount
      else return Nothing
    )
