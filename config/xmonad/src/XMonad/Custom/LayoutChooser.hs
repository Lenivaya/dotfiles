{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}

module XMonad.Custom.LayoutChooser
  ( selectLayoutByName
  , toggleLayout
  ) where

import           Data.Foldable
import           Data.Map                       ( Map )
import qualified Data.Map                      as Map
import           Data.Maybe                     ( fromMaybe )
import           XMonad                  hiding ( float
                                                , layoutHook
                                                , (|||)
                                                )
import           XMonad.Custom.Layout           ( layoutNames )
import           XMonad.Custom.Prompt
import           XMonad.Prompt
import qualified XMonad.StackSet               as Stack
import           XMonad.Util.ExtensibleState   as XState

data LayoutByName = LayoutByName

instance XPrompt LayoutByName where
  showXPrompt LayoutByName = "Layout: "

newtype LayoutHistory = LayoutHistory
  {runLayoutHistory :: Map String String}
  deriving (Typeable)

instance ExtensionClass LayoutHistory where
  initialValue = LayoutHistory Map.empty


selectLayoutByName :: XPConfig -> X ()
selectLayoutByName conf = mkXPrompt LayoutByName
                                    conf
                                    (listCompFunc conf layoutNames)
                                    go
 where
  go :: String -> X ()
  go selected = forM_ (lookup selected $ zip layoutNames layoutNames)
                      (sendMessage . JumpToLayout)


-- | Toggle between the current layout and the one given as an argument.
toggleLayout :: String -> X ()
toggleLayout name = do
  winset <- XMonad.gets windowset

  let ws = Stack.workspace . Stack.current $ winset
      wn = Stack.tag ws
      ld = description . Stack.layout $ ws

  if name == ld then restoreLayout wn else rememberAndGo wn ld
 where
    -- Restore the previous workspace.
  restoreLayout :: String -> X ()
  restoreLayout ws = do
    history <- runLayoutHistory <$> XState.get
    let ld = fromMaybe "Auto" (Map.lookup ws history)
    sendMessage (JumpToLayout ld)

  -- Remember the current workspace and jump to the requested one.
  rememberAndGo :: String -> String -> X ()
  rememberAndGo ws current = do
    history <- runLayoutHistory <$> XState.get
    XState.put (LayoutHistory $ Map.insert ws current history)
    sendMessage (JumpToLayout name)
