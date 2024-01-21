module XMonad.Custom.Actions.Keyboard where

import Control.Monad
import Data.Foldable
import Data.Functor
import XMonad
import XMonad.Actions.ShowText
import XMonad.Custom.Prompt
import XMonad.Prompt
import XMonad.Util.Run (runProcessWithInput)

setKbdLayout :: String -> X ()
setKbdLayout layout = spawn $ unwords ["xkb-switch -s", layout]

getKbdLayouts :: X [String]
getKbdLayouts = lines <$> runProcessWithInput "xkb-switch" ["-l"] ""

getCurrentLayout :: X String
getCurrentLayout = runProcessWithInput "xkb-switch" ["-p"] ""

switchToDefault = setKbdLayout "us"

{-| Temporarily switches the keyboard layout to default English
   before executing an action, and restores the previous layout afterward.
-}
wrapKbdLayout
  :: X ()
  -- ^ The action to be performed
  -> X ()
  -- ^ An XMonad action with modified keyboard layout
wrapKbdLayout action =
  getCurrentLayout >>= \layoutBeforeAction ->
    switchToDefault >> action >> setKbdLayout layoutBeforeAction

withDefaultKbdLayout action = switchToDefault >> action

data KbdLayoutPrompt = KbdLayoutPrompt

instance XPrompt KbdLayoutPrompt where
  showXPrompt _ = "Keyboard layout: "

kbdHelpConfig = def {st_font = "xft:monospace:size=20"}

selectKbdLayout :: XPConfig -> X ()
selectKbdLayout conf =
  switchToDefault >> getKbdLayouts >>= \layouts ->
    showAllLayouts layouts >> prompt layouts
  where
    showAllLayouts = flashText kbdHelpConfig 0.5 . unwords

    go layouts selected =
      forM_ (lookup selected $ zip layouts layouts) setKbdLayout

    prompt layouts =
      mkXPrompt
        KbdLayoutPrompt
        conf
        (listCompFunc conf layouts)
        (go layouts)
