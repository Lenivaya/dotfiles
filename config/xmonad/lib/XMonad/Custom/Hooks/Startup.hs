module XMonad.Custom.Hooks.Startup (
  startupHook,
) where

import Data.Maybe
import XMonad hiding (startupHook)
import XMonad.Hooks.SetWMName
import XMonad.Hooks.StatusBar
import XMonad.Util.SpawnOnce
import XMonad.Util.Cursor

startupHook :: X ()
startupHook = do
  setWMName "XMonad"
  -- setDefaultCursor xC_left_ptr
  spawn "source ~/.fehbg"
