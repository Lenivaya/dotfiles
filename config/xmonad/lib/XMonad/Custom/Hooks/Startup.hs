module XMonad.Custom.Hooks.Startup (
  startupHook,
) where

import Data.Maybe
import XMonad hiding (startupHook)
import XMonad.Custom.Actions.Keyboard
import XMonad.Hooks.SetWMName
import XMonad.Hooks.StatusBar
import XMonad.Util.Cursor
import XMonad.Util.SpawnOnce

startupHook :: X ()
startupHook = do
  -- setWMName "LG3D"
  -- setWMName "XMonad"
  -- setDefaultCursor xC_left_ptr -- sets some strange looking cursor
  spawnOnce "xsetroot -cursor_name left_ptr"
  spawnOnce "source ~/.fehbg"
  spawn "xset r rate 200 100" -- too needed for me, lets set it on every startup
  -- spawn "killall sxhkd || sxhkd"
  -- spawn "killall skippy-xd || skippy-xd --start-daemon --config-reload"
  keyboardStartupHook

-- setDefaultCursor xC_left_ptr
-- spawn "source ~/.fehbg"
