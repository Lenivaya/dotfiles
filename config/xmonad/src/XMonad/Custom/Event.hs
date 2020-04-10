module XMonad.Custom.Event
  ( handleEventHook
  )
where

import           Data.Monoid
import           XMonad                  hiding ( handleEventHook )
import           XMonad.Custom.Scratchpads
import           XMonad.Hooks.EwmhDesktops
import           XMonad.Hooks.ManageDocks
import           XMonad.Util.Loggers.NamedScratchpad
import           XMonad.Hooks.RefocusLast
import qualified Data.Map.Strict               as M

-- Keeps last focused window
myPred = refocusingIsActive <||> isFloat

handleEventHook :: Event -> X All
handleEventHook = mconcat
  [ nspTrackHook scratchpads
  , docksEventHook
  , fullscreenEventHook
  , refocusLastWhen myPred
  ]
