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

serverEventHooks =
  [serverModeEventHookF "XMONAD_SHOW_TEXT" flash']
  where
    flash' =
      flashText
        def
          { st_font = "xft:monospace:size=25"
          }
        0.5
        . wrap "  " "  "

handleEventHook :: Event -> X All
handleEventHook =
  mconcat hooks
  where
    hooks =
      [ perWindowKbdLayout,
        handleTimerEvent,
        refocusLastWhen myRefocusPred,
        mconcat $ swallower <$> ["Alacritty", "St"],
        nspTrackHook scratchpads,
        -- Hacks.windowedFullscreenFixEventHook,
        Hacks.trayerPaddingXmobarEventHook,
        Hacks.trayerAboveXmobarEventHook
        -- , onTitleChange manageHook
      ]
        ++ serverEventHooks
