{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE LambdaCase #-}
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

  -- * Startup hook
  keyboardStartupHook,
) where

import Control.Monad (unless)
import Data.Char (isSpace, toLower)
import Data.List (nub)
import Data.Maybe (listToMaybe)
import Data.Typeable (Typeable)
import Flow ((|>))
import XMonad
import XMonad.Actions.ShowText
import XMonad.Custom.Prompt
import XMonad.Custom.Utils.Keyboard (flashKeyboardChange, formatLayouts)
import XMonad.Custom.Utils.Strings (trim)
import XMonad.Prompt
import XMonad.Util.ExtensibleState as XS
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

-- | State for storing layout switch history
newtype LayoutHistory = LayoutHistory {unLayoutHistory :: [LayoutSwitch]}
  deriving (Typeable)

instance ExtensionClass LayoutHistory where
  initialValue = LayoutHistory []

-- | State for storing available keyboard layouts
newtype KbdLayoutsCache = KbdLayoutsCache {unKbdLayoutsCache :: Maybe [String]}
  deriving (Typeable)

instance ExtensionClass KbdLayoutsCache where
  initialValue = KbdLayoutsCache Nothing

-- | Create a new layout switch event
mkLayoutSwitch :: String -> Bool -> LayoutSwitch
mkLayoutSwitch layout = LayoutSwitch (cleanLayout layout)
  where
    cleanLayout = filter (not . isSpace)

-- | Update the layout switch history
recordLayoutSwitch :: LayoutSwitch -> X ()
recordLayoutSwitch switch@LayoutSwitch {..}
  | lsIsService = return () -- Don't record service switches
  | otherwise = XS.modify $ \(LayoutHistory history) ->
      LayoutHistory $
        take maxEventHistory $
          switch : filter (\LayoutSwitch {lsLayout = layout} -> layout /= lsLayout) history

-- | Get the most recent non-service layout different from the current one
getMRULayout :: String -> X (Maybe String)
getMRULayout currentLayout = do
  LayoutHistory history <- XS.get
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
  LayoutHistory history <- XS.get
  pure
    ( history
        |> filter (not . lsIsService)
        |> map lsLayout
        |> nub
    )

-- | Get available keyboard layouts
getKbdLayouts :: X [String]
getKbdLayouts = do
  KbdLayoutsCache cached <- XS.get
  case cached of
    Just layouts -> return layouts
    Nothing -> do
      layouts <-
        runProcessWithInput "xkb-switch" ["-l"] ""
          |> fmap lines
          |> fmap (filter (not . null))
      unless (null layouts) $
        XS.put $
          KbdLayoutsCache (Just layouts)
      return layouts

-- | Get the current keyboard layout
getCurrentLayout :: X String
getCurrentLayout = trim <$> runProcessWithInput "xkb-switch" ["-p"] ""

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
  current <- getCurrentLayout
  setKbdLayout True "us"
  layouts <- getKbdLayouts
  flashKeyboardChange (formatLayouts layouts (map toLower current))
  mkXPrompt KbdLayoutPrompt conf (listCompFunc conf layouts) (setKbdLayout False)

{-| Initialize keyboard layouts at startup
This function gets available keyboard layouts and initializes
the layout history with the first two layouts
-}
keyboardStartupHook :: X ()
keyboardStartupHook = do
  layouts <- getKbdLayouts
  case take 2 layouts of
    [first, second] -> do
      -- Initialize history with second layout first (older)
      -- then first layout (more recent)
      XS.put $
        LayoutHistory
          [ mkLayoutSwitch first False,
            mkLayoutSwitch second False
          ]
    [single] ->
      XS.put $
        LayoutHistory
          [mkLayoutSwitch single False]
    _ -> return ()
