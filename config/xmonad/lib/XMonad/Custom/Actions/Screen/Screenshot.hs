module XMonad.Custom.Actions.Screen.Screenshot where

import Data.Foldable
import Text.Read (readMaybe)
import XMonad
import XMonad.Prompt

data ScreenshotOption
  = Fullscreen
  | FullscreenClipboard
  | Select
  | SelectCopyToClipboard
  deriving (Enum, Bounded, Read, Show)

screenshot :: ScreenshotOption -> X ()
screenshot Fullscreen = spawn "xshot full"
screenshot FullscreenClipboard = spawn "xshot full-clipboard"
screenshot Select = spawn "xshot select"
screenshot SelectCopyToClipboard = spawn "xshot select-clipboard"

data ScreenshotPrompt = ScreenshotPrompt

instance XPrompt ScreenshotPrompt where
  showXPrompt _ = "screenshot: "

screenshotPrompt :: XPConfig -> X ()
screenshotPrompt config =
  mkXPrompt ScreenshotPrompt config (mkComplFunFromList config options) go
  where
    options = show <$> [minBound .. maxBound :: ScreenshotOption]
    go = mapM_ screenshot . readMaybe
