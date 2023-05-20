module XMonad.Custom.Event
  ( handleEventHook
  ) where

import           Data.Monoid
import           XMonad                  hiding ( handleEventHook
                                                , manageHook
                                                )
import           XMonad.Custom.Scratchpads
import           XMonad.Hooks.EwmhDesktops
import           XMonad.Hooks.ManageDocks
import           XMonad.Hooks.OnPropertyChange
import           XMonad.Hooks.PerWindowKbdLayout
import           XMonad.Hooks.RefocusLast
import           XMonad.Hooks.WindowSwallowing
import qualified XMonad.Util.Hacks             as Hacks
import           XMonad.Util.Loggers.NamedScratchpad


import           XMonad.Actions.ShowText
import           XMonad.Custom.Manage           ( manageHook )
import           XMonad.Operations

-- Keeps last focused window
myPred = refocusingIsActive <||> isFloat

handleEventHook :: Event -> X All
handleEventHook = mconcat
  [ Hacks.windowedFullscreenFixEventHook
  , perWindowKbdLayout
  , nspTrackHook scratchpads
  , swallowEventHook (className =? "Alacritty") (return True)
  , refocusLastWhen myPred
  , onTitleChange manageHook
  , handleTimerEvent
  ]
