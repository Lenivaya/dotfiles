module XMonad.Custom.Hooks.Log (
  logHook,
  topBarPP',
  botBarPP,
) where

import Data.List (
  find,
  isInfixOf,
 )
import Data.Maybe (fromMaybe)
import System.IO
import XMonad hiding (logHook)
import XMonad.Actions.CopyWindow
import XMonad.Actions.SwapPromote
import XMonad.Actions.UpdatePointer
import XMonad.Custom.Hooks.Layout (layoutNames)
import XMonad.Custom.Scratchpads
import XMonad.Custom.Theme
import XMonad.Hooks.CurrentWorkspaceOnTop
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ShowWName
import XMonad.Hooks.StatusBar.PP
import XMonad.StackSet qualified as W
-- import XMonad.Util.ClickableWorkspaces
import XMonad.Util.Loggers.NamedScratchpad
import XMonad.Util.NamedScratchpad hiding (
  namedScratchpadFilterOutWorkspace,
 )
import XMonad.Util.WorkspaceCompare
import qualified Data.Map as Map
import Data.IORef
import System.IO.Unsafe (unsafePerformIO)
import XMonad.Hooks.RefocusLast
import qualified XMonad.Util.ExtensibleState as UXS
import XMonad.Actions.Minimize
import XMonad.Util.Minimize
import XMonad.Util.Loggers
import XMonad.Custom.Utils.Loggers

-- Create a global cache for layout names
layoutNameCache :: IORef (Map.Map String String)
layoutNameCache = unsafePerformIO $ newIORef Map.empty

layoutName :: String -> String
layoutName s = if null s then "" else last (words s)

layoutName' :: String -> String
layoutName' l = unsafePerformIO $ do
  cache <- readIORef layoutNameCache
  case Map.lookup l cache of
    Just name -> return name
    Nothing -> do
      let name = layoutName l
      modifyIORef' layoutNameCache (Map.insert l name)
      return name

topBarPP :: PP
topBarPP =
  def
    { ppCurrent = xmobarColor white2 "" . xmobarFont 1 . wrap "=" "=",
      ppVisible = xmobarColor white1 "" . wrap "~" "~",
      ppHidden = xmobarColor white1 "" . wrap "-" "-",
      ppHiddenNoWindows = xmobarColor white1 "" . wrap "_" "_",
      ppUrgent = xmobarColor red2 "" . wrap "!" "!",
      ppSep = " / ",
      ppWsSep = " ",
      ppTitle = xmobarColor white1 "" . shorten 60,
      -- ppTitleSanitize = xmobarStrip,
      ppLayout = xmobarColor white1 "" . layoutName',
      ppOrder = \(ws:l:t:ex) -> [ws, l] ++ ex ++ [t],
      ppSort = (namedScratchpadFilterOutWorkspace .) <$> getSortByIndex,
      ppExtras = [windowsLogger]
}

topBarPP' :: X PP
topBarPP' = do
  c <- wsContainingCopies

  let copiesCurrent ws
        | ws `elem` c =
            xmobarColor yellow2 "" . xmobarFont 1 . wrap "*" "=" $ ws
        | otherwise =
            xmobarColor white2 "" . xmobarFont 1 . wrap "=" "=" $ ws
  let copiesHidden ws
        | ws `elem` c = xmobarColor yellow1 "" . wrap "*" "-" $ ws
        | otherwise = xmobarColor white1 "" . wrap "-" "-" $ ws
  let copiesUrgent ws
        | ws `elem` c = xmobarColor yellow2 "" . wrap "*" "!" $ ws
        | otherwise = xmobarColor white2 "" . wrap "!" "!" $ ws

  let copiesCurrentPP = xmobarColor yellow1 "" . wrap "*" "-"

  copiesPP copiesCurrentPP $ topBarPP
    { ppCurrent = copiesCurrent,
      ppHidden = copiesHidden,
      ppUrgent = copiesUrgent
    }

botBarPP :: PP
botBarPP =
  topBarPP
    { ppCurrent = const "",
      ppVisible = const "",
      ppHidden = const "",
      ppHiddenNoWindows = const "",
      ppUrgent = const "",
      ppTitle = const "",
      ppLayout = const ""
    }

logHook :: X ()
logHook = do
  masterHistoryHook
  updatePointer (0.5, 0.5) (0, 0)
  refocusLastLogHook
  nsHideOnFocusLoss scratchpads
  -- nsSingleScratchpadPerWorkspace scratchpads
  showWNameLogHook def
  -- currentWorkspaceOnTop
  -- fadeWindowsLogHook myFadeHook
