module XMonad.Custom.Hooks.Screens (
  myRandrChangeHook,
) where

import XMonad.Actions.ShowText
import XMonad.Core

flash' = flashText def 0.5

myRandrChangeHook :: X ()
myRandrChangeHook = do
  flash' "Screen change"
  pure ()

-- spawn "autorandr --change"
-- spawn "~/.fehbg"
