module XMonad.Custom.Screenshot where

import Data.Foldable
import Text.Read (readMaybe)
import XMonad
import XMonad.Custom.Misc
import XMonad.Custom.Prompt
import XMonad.Prompt

data ScreenshotOption
  = Fullscreen
  | Select
  | SelectCopyToClipboard
  deriving (Enum, Bounded, Read, Show)

screenshot :: ScreenshotOption -> X ()
screenshot Fullscreen = spawn "xshot.sh"
screenshot Select = spawn "xshot-select.sh"
screenshot SelectCopyToClipboard = spawn "xshot-select-clipboard.sh"

data ScreenshotPrompt = ScreenshotPrompt

instance XPrompt ScreenshotPrompt where
  showXPrompt _ = "screenshot: "

screenshotPrompt :: XPConfig -> X ()
screenshotPrompt config =
  mkXPrompt ScreenshotPrompt config (listCompFunc config options) go
  where
    options = show <$> [minBound .. maxBound :: ScreenshotOption]
    go option = forM_ (readMaybe option) screenshot
