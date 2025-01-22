module XMonad.Custom.Actions.ScratchpadChooser where

import XMonad
import XMonad.Custom.Prompt
import XMonad.Custom.Scratchpads (scratchpads)
import XMonad.Prompt
import XMonad.Util.NamedScratchpad

-- Prompt
data ScratchpadByName = ScratchpadByName

instance XPrompt ScratchpadByName where
  showXPrompt _ = "Scratchpad: "

-- Get list of scratchpad names from the scratchpads list
scratchpadNames :: [String]
scratchpadNames = map name scratchpads

-- Prompt to select and run a scratchpad
selectScratchpadByName :: XPConfig -> X ()
selectScratchpadByName conf =
  mkXPrompt
    ScratchpadByName
    conf
    (mkComplFunFromList' conf scratchpadNames)
    (namedScratchpadAction scratchpads)
