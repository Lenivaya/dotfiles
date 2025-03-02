{-# LANGUAGE BangPatterns #-}
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

import Control.Monad (unless, void, when)
import Data.Char (isSpace, toLower)
import Data.List (find, nub)
import Data.Maybe (fromMaybe)
import Data.Typeable (Typeable)
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
  deriving (Show, Read, Eq, Typeable)

-- | Maximum number of events to store
maxEventHistory :: Int
maxEventHistory = 5

-- | State for storing layout switch history
newtype LayoutHistory = LayoutHistory {unLayoutHistory :: [LayoutSwitch]}
  deriving (Show, Read, Typeable)

instance ExtensionClass LayoutHistory where
  initialValue = LayoutHistory []
  extensionType = PersistentExtension

-- | State for storing available keyboard layouts
newtype KbdLayoutsCache = KbdLayoutsCache {unKbdLayoutsCache :: Maybe [String]}
  deriving (Show, Read, Typeable)

instance ExtensionClass KbdLayoutsCache where
  initialValue = KbdLayoutsCache Nothing
  extensionType = PersistentExtension

-- | Create a new layout switch event with normalized layout name
mkLayoutSwitch :: String -> Bool -> LayoutSwitch
mkLayoutSwitch layout isService =
  let !normalizedLayout = filter (not . isSpace) layout
  in  LayoutSwitch normalizedLayout isService

{-| Update the layout switch history
Only records non-service switches and maintains a unique history
-}
recordLayoutSwitch :: LayoutSwitch -> X ()
recordLayoutSwitch switch
  | lsIsService switch = return () -- Don't record service switches
  | otherwise = XS.modify $ \(LayoutHistory history) ->
      let !newHistory =
            take maxEventHistory $
              switch : filter (\s -> lsLayout s /= lsLayout switch) history
      in  LayoutHistory newHistory

-- | Get the most recent non-service layout different from the current one
getMRULayout :: String -> X (Maybe String)
getMRULayout currentLayout = do
  LayoutHistory history <- XS.get
  return $ lsLayout <$> find isValidPreviousLayout history
  where
    isValidPreviousLayout s = lsLayout s /= currentLayout && not (lsIsService s)

-- | Set keyboard layout with history tracking
setKbdLayout :: Bool -> String -> X ()
setKbdLayout isService layout = do
  let !switch = mkLayoutSwitch layout isService
  unless isService $ recordLayoutSwitch switch
  spawn $ "xkb-switch -s " ++ lsLayout switch

-- | Switch to the most recently used non-service layout
switchToMRUKbdLayout :: X ()
switchToMRUKbdLayout = do
  current <- getCurrentLayout
  getMRULayout current >>= \case
    Just layout -> setKbdLayout False layout
    Nothing -> return ()

{-| Wrap an action with keyboard layout switching
Temporarily switches to US layout, performs the action, then switches back
-}
wrapKbdLayout :: X () -> X ()
wrapKbdLayout action = do
  layoutBeforeAction <- getCurrentLayout
  when (layoutBeforeAction /= "us") $ setKbdLayout True "us"
  action
  when (layoutBeforeAction /= "us") $ setKbdLayout True layoutBeforeAction

-- | Get recent non-service layouts (for debugging)
getRecentLayouts :: X [String]
getRecentLayouts = do
  LayoutHistory history <- XS.get
  return $ nub $ map lsLayout $ filter (not . lsIsService) history

-- | Get available keyboard layouts with caching
getKbdLayouts :: X [String]
getKbdLayouts = do
  KbdLayoutsCache cached <- XS.get
  case cached of
    Just layouts -> return layouts
    Nothing -> do
      output <- runProcessWithInput "xkb-switch" ["-l"] ""
      let !layouts = filter (not . null) (lines output)
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

-- | Prompt the user to select a keyboard layout
selectKbdLayout :: XPConfig -> X ()
selectKbdLayout conf = do
  current <- getCurrentLayout
  current' <- map toLower <$> getCurrentLayout
  layouts <- getKbdLayouts

  -- Only switch to US if we're not already using it
  when (current' /= "us") $ void $ setKbdLayout True "us"

  -- Show available layouts with current one highlighted
  flashKeyboardChange (formatLayouts layouts current')

  -- Create completion function and prompt
  mkXPrompt
    KbdLayoutPrompt
    conf
    (mkComplFunFromList conf layouts)
    (setKbdLayout False)

{-| Initialize keyboard layouts at startup
    Gets available keyboard layouts and initializes the layout history
    with the first two layouts for quick switching
-}
keyboardStartupHook :: X ()
keyboardStartupHook = do
  layouts <- getKbdLayouts
  case layouts of
    (first : second : _) ->
      XS.put $
        LayoutHistory
          [ mkLayoutSwitch first False,
            mkLayoutSwitch second False
          ]
    [single] ->
      XS.put $ LayoutHistory [mkLayoutSwitch single False]
    _ -> return ()
