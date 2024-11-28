module XMonad.Custom.Hooks.Startup (
  startupHook,
) where

import Data.Maybe
import XMonad hiding (startupHook)
import XMonad.Hooks.SetWMName
import XMonad.Hooks.StatusBar
import XMonad.Util.Cursor
import XMonad.Util.SpawnOnce

startupHook :: X ()
startupHook = do
  setWMName "LG3D"
  spawn "source ~/.fehbg"
  -- setWMName "XMonad"

-- setDefaultCursor xC_left_ptr
-- spawn "source ~/.fehbg"
