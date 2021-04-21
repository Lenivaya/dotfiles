module XMonad.Custom.Event
  ( handleEventHook
  ) where

import qualified Data.Map.Strict               as M
import           Data.Monoid
import           XMonad                  hiding ( handleEventHook )
import           XMonad.Custom.Scratchpads
import           XMonad.Hooks.EwmhDesktops
import           XMonad.Hooks.ManageDocks
import           XMonad.Hooks.RefocusLast
import           XMonad.Util.Loggers.NamedScratchpad

-- Keeps last focused window
myPred = refocusingIsActive <||> isFloat

handleEventHook :: Event -> X All
handleEventHook = mconcat
  [ nspTrackHook scratchpads
  , docksEventHook
  , fullscreenEventHook
  , refocusLastWhen myPred
  ]
