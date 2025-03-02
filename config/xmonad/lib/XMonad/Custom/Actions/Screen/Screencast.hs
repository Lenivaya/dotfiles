module XMonad.Custom.Actions.Screen.Screencast where

import Data.Foldable
import Text.Read (readMaybe)
import XMonad
import XMonad.Prompt

data ScreencastOption
  = RecordToMP4
  | RecordToGIF
  deriving (Enum, Bounded, Read, Show)

screencast :: ScreencastOption -> X ()
screencast RecordToMP4 = spawn "scrrec -s ~/recordings/$(date +%F-%T).mp4"
screencast RecordToGIF = spawn "scrrec -s ~/recordings/$(date +%F-%T).gif"

data ScreencastPrompt = ScreencastPrompt

instance XPrompt ScreencastPrompt where
  showXPrompt _ = "screencast: "

screencastPrompt :: XPConfig -> X ()
screencastPrompt config =
  mkXPrompt ScreencastPrompt config (mkComplFunFromList config options) go
  where
    options = show <$> [minBound .. maxBound :: ScreencastOption]
    go = mapM_ screencast . readMaybe
