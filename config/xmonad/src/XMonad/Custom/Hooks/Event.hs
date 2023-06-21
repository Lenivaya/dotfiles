module XMonad.Custom.Hooks.Event (
  handleEventHook,
) where

import Data.Monoid
import XMonad hiding (
  handleEventHook,
  manageHook,
 )
import XMonad.Actions.ShowText
import XMonad.Custom.Manage.ManageHook (manageHook)
import XMonad.Custom.Scratchpads
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.OnPropertyChange
import XMonad.Hooks.PerWindowKbdLayout
import XMonad.Hooks.RefocusLast
import XMonad.Hooks.WindowSwallowing
import XMonad.Operations
import XMonad.Util.Hacks qualified as Hacks
import XMonad.Util.Loggers.NamedScratchpad

-- Keeps last focused window
myRefocusPred = refocusingIsActive <||> isFloat

swallower prog = swallowEventHook (className =? prog) (pure True)

handleEventHook :: Event -> X All
handleEventHook =
  mconcat
    [ Hacks.windowedFullscreenFixEventHook
    , perWindowKbdLayout
    , nspTrackHook scratchpads
    , mconcat $ swallower <$> ["Alacritty", "St"]
    , refocusLastWhen myRefocusPred
    , onTitleChange manageHook
    , handleTimerEvent
    , Hacks.trayerPaddingXmobarEventHook
    , Hacks.trayerAboveXmobarEventHook
    ]
