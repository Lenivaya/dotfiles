module XMonad.Custom.Startup
    ( startupHook
    ) where

import           Control.Monad
import           Data.Maybe
import           XMonad                  hiding ( startupHook )
import           XMonad.Custom.Log
import           XMonad.Hooks.SetWMName
import           XMonad.Util.SpawnOnce

startupHook :: X ()
startupHook = do
  -- spawnXmobar
  -- docksStartupHook
    spawn "betterlockscreen -w"
    setWMName "xmonad"
