{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module XMonad.Custom.Actions.Keyboard (
  -- * Types
  LayoutSwitch (..),

  -- * Core functions
  setKbdLayout,
  switchToMRUKbdLayout,
  selectKbdLayout,
  wrapKbdLayout,

  -- * Utility functions
  getCurrentLayout,
  getKbdLayouts,
  getRecentLayouts,
) where

import Control.Monad (unless)
import Data.Char (isSpace)
import Data.IORef
import Data.List (nub)
import Data.Maybe (listToMaybe)
import Flow ((|>))
import System.IO.Unsafe (unsafePerformIO)
import XMonad
import XMonad.Actions.ShowText
import XMonad.Custom.Prompt
import XMonad.Prompt
import XMonad.Util.Run (runProcessWithInput)

-- | Represents a keyboard layout switch event
data LayoutSwitch = LayoutSwitch
  { -- | The layout that was switched to
    lsLayout :: !String,
    -- | Whether this was a service switch (e.g., temporary English for input)
    lsIsService :: !Bool
  }
  deriving (Show, Eq)

-- | Maximum number of events to store
maxEventHistory :: Int
maxEventHistory = 5

-- | Cache for storing layout switch history
{-# NOINLINE layoutSwitchHistory #-}
layoutSwitchHistory :: IORef [LayoutSwitch]
layoutSwitchHistory = unsafePerformIO $ newIORef []

-- | Create a new layout switch event
mkLayoutSwitch :: String -> Bool -> LayoutSwitch
mkLayoutSwitch layout = LayoutSwitch (cleanLayout layout)
  where
    cleanLayout = filter (not . isSpace)

-- | Update the layout switch history
recordLayoutSwitch :: LayoutSwitch -> X ()
recordLayoutSwitch switch@LayoutSwitch {..}
  | lsIsService = return () -- Don't record service switches
  | otherwise = liftIO $ modifyIORef layoutSwitchHistory updateHistory
  where
    updateHistory history =
      take maxEventHistory $
        switch
          : filter (\LayoutSwitch {lsLayout = layout} -> layout /= lsLayout) history

-- | Get the most recent non-service layout different from the current one
getMRULayout :: String -> X (Maybe String)
getMRULayout currentLayout = do
  history <- liftIO $ readIORef layoutSwitchHistory
  pure
    ( history
        |> filter (\LayoutSwitch {lsLayout = layout} -> layout /= currentLayout)
        |> filter (not . lsIsService)
        |> listToMaybe
        |> fmap lsLayout
    )

-- | Set keyboard layout with history tracking
setKbdLayout :: Bool -> String -> X ()
setKbdLayout isService layout = do
  let switch = mkLayoutSwitch layout isService
  unless isService $ recordLayoutSwitch switch
  spawn $ "xkb-switch -s " ++ lsLayout switch

-- | Switch to the most recently used non-service layout
switchToMRUKbdLayout :: X ()
switchToMRUKbdLayout = do
  current <- mkLayoutSwitch <$> getCurrentLayout <*> pure False
  getMRULayout (lsLayout current) >>= \case
    Just layout -> setKbdLayout False layout
    Nothing -> return ()

-- | Wrap an action with keyboard layout switching
wrapKbdLayout :: X () -> X ()
wrapKbdLayout action = do
  layoutBeforeAction <- getCurrentLayout
  setKbdLayout True "us" -- Mark as service switch
  action
  setKbdLayout True layoutBeforeAction -- Mark as service switch

-- | Get recent non-service layouts (for debugging)
getRecentLayouts :: X [String]
getRecentLayouts = do
  history <- liftIO $ readIORef layoutSwitchHistory
  pure
    ( history
        |> filter (not . lsIsService)
        |> map lsLayout
        |> nub
    )

-- | Get available keyboard layouts
getKbdLayouts :: X [String]
getKbdLayouts =
  runProcessWithInput "xkb-switch" ["-l"] ""
    |> fmap lines
    |> fmap (filter (not . null))

-- | Get the current keyboard layout
getCurrentLayout :: X String
getCurrentLayout = runProcessWithInput "xkb-switch" ["-p"] ""

-- | Prompt for selecting a keyboard layout
data KbdLayoutPrompt = KbdLayoutPrompt

instance XPrompt KbdLayoutPrompt where
  showXPrompt _ = "Keyboard layout: "

-- | Configuration for displaying the keyboard layout help text
kbdHelpConfig :: ShowTextConfig
kbdHelpConfig = def {st_font = "xft:monospace:size=20"}

-- | Prompt the user to select a keyboard layout
selectKbdLayout :: XPConfig -> X ()
selectKbdLayout conf = do
  setKbdLayout True "us"
  layouts <- getKbdLayouts
  flashText kbdHelpConfig 0.5 (unwords layouts)
  mkXPrompt KbdLayoutPrompt conf (listCompFunc conf layouts) (setKbdLayout False)
