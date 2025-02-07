{-# LANGUAGE ImportQualifiedPost #-}

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
import XMonad.Custom.Hooks.KeyboardChangeEvent
import XMonad.Custom.Manage.ManageHook (manageHook)
import XMonad.Custom.Prompt
import XMonad.Custom.Scratchpads
import XMonad.Hooks.FloatConfigureReq
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.OnPropertyChange
import XMonad.Hooks.PerWindowKbdLayout
import XMonad.Hooks.RefocusLast
import XMonad.Hooks.ServerMode
import XMonad.Hooks.StatusBar.PP (wrap)
import XMonad.Hooks.WindowSwallowing
import XMonad.Layout.Fullscreen
import XMonad.Operations
import XMonad.Util.Hacks qualified as Hacks
import XMonad.Util.Loggers.NamedScratchpad

myRefocusPred = refocusingIsActive <||> isFloat

handleEventHook :: Event -> X All
handleEventHook =
  mconcat hooks
  where
    hooks =
      -- serverEventHooks ++
      [ -- floatConfReqHook myFloatConfReqHook,
        -- nspTrackHook scratchpads,
        handleTimerEvent,
        keyboardChangeEventHook,
        -- perWindowKbdLayout,
        refocusLastWhen myRefocusPred,
        Hacks.trayerAboveXmobarEventHook,
        Hacks.trayerPaddingXmobarEventHook
        -- Hacks.windowedFullscreenFixEventHook
        -- fixSteamFlicker,
        -- mconcat $ swallower <$> ["Alacritty", "St"]
        -- , onTitleChange manageHook
      ]
