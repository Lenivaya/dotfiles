module XMonad.Custom.Event
  ( handleEventHook
  ) where

import qualified Data.Map.Strict               as M
import           Data.Monoid
import           XMonad                  hiding ( handleEventHook
                                                , manageHook
                                                )
import           XMonad.Custom.Scratchpads
import           XMonad.Hooks.DynamicProperty
import           XMonad.Hooks.EwmhDesktops
import           XMonad.Hooks.ManageDocks
import           XMonad.Hooks.RefocusLast
import           XMonad.Hooks.WindowSwallowing
import qualified XMonad.Util.Hacks             as Hacks
import           XMonad.Util.Loggers.NamedScratchpad

import           XMonad.Custom.Manage           ( manageHook )

-- Keeps last focused window
myPred = refocusingIsActive <||> isFloat

handleEventHook :: Event -> X All
handleEventHook = mconcat
  [ swallowEventHook (className =? "Alacritty") (return True)
  , nspTrackHook scratchpads
  -- , docksEventHook
  -- , fullscreenEventHook
  , Hacks.windowedFullscreenFixEventHook
  , refocusLastWhen myPred
  -- , dynamicTitle manageHook
  ]
