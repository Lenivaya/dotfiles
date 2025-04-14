{-# LANGUAGE ImportQualifiedPost #-}

module XMonad.Custom.Actions.DoPrompt (
  doPrompt,
  doSomethingPrompt,
) where

import Data.Char (toLower)
import Data.Map qualified as M
import XMonad
import XMonad.Prompt

import XMonad.Custom.Actions.DoActions
import XMonad.Custom.Actions.Keyboard (wrapKbdLayout)
import XMonad.Custom.Prompt

data DoPrompt = DoPrompt

instance XPrompt DoPrompt where
  showXPrompt _ = "Do: "
  commandToComplete _ = id
  nextCompletion _ = getNextCompletion

-- | Show a prompt with available actions and execute the selected one
doPrompt :: XPConfig -> X ()
doPrompt config = wrapKbdLayout $ doPromptRaw config

-- | Internal function to avoid nesting wrapKbdLayout calls
doPromptRaw :: XPConfig -> X ()
doPromptRaw config = do
  let actions = allActions
      actionMap = M.fromList [(actionName a, a) | a <- actions]
      actionNames = map actionName actions
  mkXPrompt DoPrompt config (mkComplFunFromList' config actionNames) $ \chosen ->
    maybe (return ()) doAction (M.lookup chosen actionMap)

-- | Alternative name for doPrompt to be used in keybindings
doSomethingPrompt :: XPConfig -> X ()
doSomethingPrompt = doPrompt
