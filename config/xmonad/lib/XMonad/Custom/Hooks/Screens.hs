module XMonad.Custom.Hooks.Screens (
  myRandrChangeHook,
) where

import XMonad.Core

myRandrChangeHook :: X ()
myRandrChangeHook = do
  pure ()
  -- spawn "autorandr --change"
  -- spawn "~/.fehbg"
