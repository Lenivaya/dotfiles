module XMonad.Custom.Actions.Keyboard where

import Control.Monad
import Data.IORef
import System.IO.Unsafe (unsafePerformIO)
import XMonad
import XMonad.Actions.ShowText
import XMonad.Custom.Prompt
import XMonad.Prompt
import XMonad.Util.Run (runProcessWithInput)

-- | Set the keyboard layout using xkb-switch
setKbdLayout :: String -> X ()
setKbdLayout layout = spawn $ "xkb-switch -s " ++ layout

-- | Cache for storing the list of keyboard layouts
{-# NOINLINE cachedLayouts #-}
cachedLayouts :: IORef (Maybe [String])
cachedLayouts = unsafePerformIO $ newIORef Nothing

{-| Get the list of available keyboard layouts
This function caches the result to avoid repeated system calls
-}
getKbdLayouts :: X [String]
getKbdLayouts = do
  cached <- liftIO $ readIORef cachedLayouts
  case cached of
    Just layouts -> return layouts
    Nothing -> do
      layouts <- lines <$> runProcessWithInput "xkb-switch" ["-l"] ""
      liftIO $ writeIORef cachedLayouts (Just layouts)
      return layouts

-- | Get the current keyboard layout
getCurrentLayout :: X String
getCurrentLayout = runProcessWithInput "xkb-switch" ["-p"] ""

-- | Switch to the default keyboard layout (US)
switchToDefault :: X ()
switchToDefault = setKbdLayout "us"

{-| Wrap an action with keyboard layout switching
Switches to the default layout before the action and restores the original layout after
-}
wrapKbdLayout
  :: X ()
  -- ^ The action to be performed
  -> X ()
  -- ^ An XMonad action with modified keyboard layout
wrapKbdLayout action = do
  layoutBeforeAction <- getCurrentLayout
  switchToDefault
  action
  setKbdLayout layoutBeforeAction

-- | Perform an action with the default keyboard layout
withDefaultKbdLayout :: X () -> X ()
withDefaultKbdLayout action = switchToDefault >> action

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
  switchToDefault
  layouts <- getKbdLayouts
  flashText kbdHelpConfig 0.5 (unwords layouts)
  mkXPrompt KbdLayoutPrompt conf (listCompFunc conf layouts) setKbdLayout
