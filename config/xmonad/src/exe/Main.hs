module Main where

import           XMonad
import           XMonad.Custom.Config

main :: IO ()
main = myConfig >>= xmonad
