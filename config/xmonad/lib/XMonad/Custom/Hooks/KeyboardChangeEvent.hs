{-|
Module      : XMonad.Custom.Hooks.KeyboardChangeEvent
Description : Keyboard layout change event handler for XMonad
Copyright   : (c) Danylo Osipchuk, 2024
License     : BSD3-style (see LICENSE)
Maintainer  : danylo.osipchuk@proton.me

This module provides functionality to display keyboard layout changes in XMonad.
It shows a temporary notification with all available keyboard layouts, highlighting
the currently active one in uppercase and brackets.

Example output when switching to Ukrainian layout with US, RU, and UA layouts available:
@
us [UA] ru
@
-}
module XMonad.Custom.Hooks.KeyboardChangeEvent (
  -- * Main Hook
  keyboardChangeEventHook,

  -- * Helper Functions
  keyboardChangeEventHookHandler,
) where

import Data.Char (toLower, toUpper)
import XMonad
import XMonad.Actions.ShowText
import XMonad.Custom.Actions.Keyboard
import XMonad.Custom.Utils.Keyboard
import XMonad.Hooks.ServerMode
import XMonad.Hooks.StatusBar.PP (wrap)
import XMonad.Prelude

{-| Main event hook that listens for keyboard layout change events
To be used in the handleEventHook of your XMonad configuration
-}
keyboardChangeEventHook :: Event -> X All
keyboardChangeEventHook =
  serverModeEventHookF
    "XMONAD_SHOW_KEYBOARD_CHANGE"
    keyboardChangeEventHookHandler

{-| Handles keyboard layout change events by displaying a notification
showing all available layouts with the current one highlighted

The handler:
1. Gets all available keyboard layouts
2. Formats each layout string (current layout is uppercase and bracketed)
3. Joins layouts with spaces and displays the result
-}
keyboardChangeEventHookHandler :: String -> X ()
keyboardChangeEventHookHandler newLayout = do
  layouts <- getKbdLayouts
  flashKeyboardChange $ formatLayouts layouts (map toLower newLayout)
