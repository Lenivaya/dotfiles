module XMonad.Custom.Screens
  ( myRandrChangeHook
  ) where

import           XMonad.Core

myRandrChangeHook :: X ()
myRandrChangeHook = spawn "autorandr --change"
