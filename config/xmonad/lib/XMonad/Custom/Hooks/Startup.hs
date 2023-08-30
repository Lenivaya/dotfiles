module XMonad.Custom.Hooks.Startup (
  startupHook,
) where

import Data.Maybe
import XMonad hiding (startupHook)
import XMonad.Hooks.SetWMName
import XMonad.Hooks.StatusBar
import XMonad.Util.SpawnOnce

startupHook :: X ()
startupHook = do
  setWMName "XMonad"
  spawn "betterlockscreen -w"
  spawn "$XMONAD_CONFIG_DIR/scripts/keyboard-listener"
