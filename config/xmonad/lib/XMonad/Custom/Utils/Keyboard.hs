module XMonad.Custom.Utils.Keyboard where

import Data.Char (toLower, toUpper)
import XMonad
import XMonad.Actions.ShowText
import XMonad.Hooks.StatusBar.PP (wrap)

{-| Formats a list of keyboard layouts into a single string, highlighting the current layout.

The function takes a list of layout names and the current layout name, then returns
a space-separated string where the current layout is uppercase and wrapped in brackets.

Example:
>>> formatLayouts ["us", "ua", "ru"] "ua"
"us [UA] ru"
-}
formatLayouts :: [String] -> String -> String
formatLayouts layouts current =
  unwords $ map (formatLayout current) layouts

{-| Formats a single keyboard layout string based on whether it's currently active.

If the layout matches the current layout (case-insensitive):
  - Converts the layout to uppercase
  - Wraps it in square brackets
Otherwise:
  - Returns the layout string unchanged

Example:
>>> formatLayout "ua" "ua"
"[UA]"
>>> formatLayout "ua" "us"
"us"
-}
formatLayout :: String -> String -> String
formatLayout current layout
  | map toLower layout == current = "[" ++ map toUpper layout ++ "]"
  | otherwise = layout

-- | Displays a notification with custom font settings
flashKeyboardChange :: String -> X ()
flashKeyboardChange = flashText def {st_font = "xft:monospace:size=30"} 0.5 . wrap "  " "  "
