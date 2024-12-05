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
import XMonad.Util.ClickableWorkspaces
import XMonad.Util.Loggers.NamedScratchpad
import XMonad.Util.NamedScratchpad hiding (
  namedScratchpadFilterOutWorkspace,
 )
import XMonad.Util.WorkspaceCompare
import qualified Data.Map as Map
import Data.IORef
import System.IO.Unsafe (unsafePerformIO)


-- layoutIcon :: String -> String
-- layoutIcon l | "BSP" `isInfixOf` l         = " <fn=1>\57654</fn>"
--              | "Circle" `isInfixOf` l      = " <fn=1>\57521</fn>"
--              | "Tall" `isInfixOf` l        = " <fn=1>\57346</fn>"
--              | "ThreeColMid" `isInfixOf` l = " <fn=1>\57377</fn>"
--              | "OneBig" `isInfixOf` l      = " <fn=1>\57377</fn>"
--              | otherwise                   = ""

-- Create a global cache for layout names
layoutNameCache :: IORef (Map.Map String String)
layoutNameCache = unsafePerformIO $ newIORef Map.empty

layoutName :: String -> String
layoutName l = unsafePerformIO $ do
  cache <- readIORef layoutNameCache
  case Map.lookup l cache of
    Just name -> return name
    Nothing -> do
      let name = fromMaybe "" $ find (`isInfixOf` l) layoutNames
      modifyIORef' layoutNameCache (Map.insert l name)
      return name

windowCount =
  Just
    . xmobarColor white2 ""
    . xmobarFont 2
    . wrap "[" "]"
    . show
    . length
    . W.integrate'
    . W.stack
    . W.workspace
    . W.current
    . windowset
    <$> get

topBarPP :: PP
topBarPP =
  def
    { ppCurrent = xmobarColor white2 "" . xmobarFont 2 . wrap "=" "=",
      ppVisible = xmobarColor white1 "" . wrap "~" "~",
      ppHidden = xmobarColor white1 "" . wrap "-" "-",
      ppHiddenNoWindows = xmobarColor white1 "" . wrap "_" "_",
      ppUrgent = xmobarColor red2 "" . wrap "!" "!",
      ppSep = " / ",
      ppWsSep = " ",
      ppTitle = xmobarColor white1 "" . shorten 60,
      ppTitleSanitize = xmobarStrip,
      ppLayout = xmobarColor white1 "" . layoutName,
      -- , ppOrder           = id
      ppOrder = \[ws, l, t, ex] -> [ws, l, ex, t],
      ppSort = (namedScratchpadFilterOutWorkspace .) <$> getSortByIndex,
      ppExtras = [windowCount]
    }

topBarPP' :: X PP
topBarPP' = do
  c <- wsContainingCopies

  let copiesCurrent ws
        | ws `elem` c =
            xmobarColor yellow2 "" . xmobarFont 2 . wrap "*" "=" $ ws
        | otherwise =
            xmobarColor white2 "" . xmobarFont 2 . wrap "=" "=" $ ws
  let copiesHidden ws
        | ws `elem` c = xmobarColor yellow1 "" . wrap "*" "-" $ ws
        | otherwise = xmobarColor white1 "" . wrap "-" "-" $ ws
  let copiesUrgent ws
        | ws `elem` c = xmobarColor yellow2 "" . wrap "*" "!" $ ws
        | otherwise = xmobarColor white2 "" . wrap "!" "!" $ ws

  let copiesCurrentPP = xmobarColor yellow1 "" . wrap "*" "-"

  clickablePP
    topBarPP
      { ppCurrent = copiesCurrent,
        ppHidden = copiesHidden,
        ppUrgent = copiesUrgent
      }
    >>= copiesPP copiesCurrentPP

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
  updatePointer (0.5, 0.5) (0, 0)
  masterHistoryHook
  currentWorkspaceOnTop
  showWNameLogHook def
  -- fadeWindowsLogHook myFadeHook
  -- nsHideOnFocusLoss scratchpads
