module XMonad.Custom.Hooks.Event (
  handleEventHook,
) where

import Data.Char (toUpper)
import Data.Monoid

import XMonad hiding (
  handleEventHook,
  manageHook,
 )
import XMonad.Actions.ShowText
import XMonad.Custom.Manage.ManageHook (manageHook)
import XMonad.Custom.Prompt
import XMonad.Custom.Scratchpads
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.OnPropertyChange
import XMonad.Hooks.PerWindowKbdLayout
import XMonad.Hooks.RefocusLast
import XMonad.Hooks.ServerMode
import XMonad.Hooks.StatusBar.PP (wrap)
import XMonad.Hooks.WindowSwallowing
import XMonad.Operations
import XMonad.Util.Hacks qualified as Hacks
import XMonad.Util.Loggers.NamedScratchpad

myRefocusPred = refocusingIsActive <||> isFloat
swallower prog = swallowEventHook (className =? prog) (pure True)

handleEventHook :: Event -> X All
handleEventHook =
  mconcat
    [ perWindowKbdLayout
    , handleTimerEvent
    , Hacks.windowedFullscreenFixEventHook
    , refocusLastWhen myRefocusPred
    , mconcat $ swallower <$> ["Alacritty", "St"]
    , nspTrackHook scratchpads
    -- , onTitleChange manageHook
    , Hacks.trayerPaddingXmobarEventHook
    , Hacks.trayerAboveXmobarEventHook
    , serverModeEventHookF
        "XMONAD_SHOW_TEXT"
        ( flashText
            def
              { st_font = "xft:monospace:size=25"
              }
            0.5
            . wrap "  " "  "
        )
    ]
