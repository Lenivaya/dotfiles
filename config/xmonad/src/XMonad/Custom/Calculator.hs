module XMonad.Custom.Calculator where

import Control.Monad
import Data.Char
import Data.List
import XMonad
import XMonad.Prompt
import XMonad.Prompt.Input
import XMonad.Util.Run (runProcessWithInput)

calcPrompt :: XPConfig -> String -> X ()
calcPrompt config answer =
  inputPrompt config (trim answer) ?+ (doCalc >=> calcPrompt config)
  where
    trim = dropWhileEnd isSpace

doCalc :: MonadIO m => String -> m String
doCalc input = runProcessWithInput "qalc" [input] ""
