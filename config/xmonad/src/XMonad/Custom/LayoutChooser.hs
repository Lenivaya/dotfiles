{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}

module XMonad.Custom.LayoutChooser where

import Data.Foldable
import Data.Maybe (fromMaybe)
import XMonad
import XMonad.Custom.Layout (layoutNames)
import XMonad.Custom.Prompt
import XMonad.Prompt
import XMonad.StackSet qualified as Stack
import XMonad.Util.ExtensibleState as XState

data LayoutByName = LayoutByName

instance XPrompt LayoutByName where
  showXPrompt LayoutByName = "Layout: "

selectLayoutByName :: XPConfig -> X ()
selectLayoutByName conf =
  mkXPrompt
    LayoutByName
    conf
    (listCompFunc conf layoutNames)
    go
  where
    go selected =
      forM_
        (lookup selected $ zip layoutNames layoutNames)
        (sendMessage . JumpToLayout)
