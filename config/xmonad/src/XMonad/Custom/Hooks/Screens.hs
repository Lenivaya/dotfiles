module XMonad.Custom.Hooks.Screens (
  myRandrChangeHook,
) where

import XMonad.Core

myRandrChangeHook :: X ()
myRandrChangeHook = do
  spawn "autorandr --change"
  spawn "betterlockscreen -w"
