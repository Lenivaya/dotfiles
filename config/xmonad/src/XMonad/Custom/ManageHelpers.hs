module XMonad.Custom.ManageHelpers (
  centerFloat,
) where

import XMonad hiding (manageHook)
import XMonad.Hooks.ManageHelpers
import XMonad.StackSet as W
import XMonad.Util.NamedScratchpad

centerFloat :: Rational -> Rational -> ManageHook
centerFloat w h = doRectFloat $ W.RationalRect x y w h
  where
    x = (1 - w) / 2
    y = (1 - h) / 2
