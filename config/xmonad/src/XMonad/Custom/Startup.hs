module XMonad.Custom.Startup
  ( startupHook
  ) where

import           Data.Maybe
import           XMonad                  hiding ( startupHook )
import           XMonad.Hooks.SetWMName
import           XMonad.Hooks.StatusBar
import           XMonad.Util.SpawnOnce

startupHook :: X ()
startupHook = do
  spawn "betterlockscreen -w"
  setWMName "xmonad"
