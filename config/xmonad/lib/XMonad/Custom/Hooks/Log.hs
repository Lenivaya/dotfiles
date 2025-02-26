{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ImportQualifiedPost #-}

module XMonad.Custom.Hooks.Log (
  logHook,
  topBarPP',
  botBarPP,
) where

import Data.List (
  find,
  isInfixOf,
 )
import Data.Map qualified as Map
import Data.Maybe (fromMaybe)
import Data.Typeable (Typeable)
import System.IO
import XMonad hiding (logHook)
import XMonad.Actions.CopyWindow
import XMonad.Actions.Minimize
import XMonad.Actions.SwapPromote
import XMonad.Actions.UpdatePointer
import XMonad.Custom.Hooks.Layout (layoutNames)
import XMonad.Custom.Scratchpads
import XMonad.Custom.Theme
import XMonad.Custom.Utils.Loggers
import XMonad.Hooks.CurrentWorkspaceOnTop
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.RefocusLast
import XMonad.Hooks.ShowWName
import XMonad.Hooks.StatusBar.PP
import XMonad.StackSet qualified as W
import XMonad.Util.ExtensibleState qualified as XS
import XMonad.Util.Loggers
import XMonad.Util.Loggers.NamedScratchpad
import XMonad.Util.Minimize
import XMonad.Util.NamedScratchpad hiding (
  namedScratchpadFilterOutWorkspace,
 )
import XMonad.Util.WorkspaceCompare

-- | State type for layout name cache
newtype LayoutNameCache = LayoutNameCache {unLayoutNameCache :: Map.Map String String}
  deriving (Typeable)

instance ExtensionClass LayoutNameCache where
  initialValue = LayoutNameCache Map.empty

layoutName :: String -> String
layoutName s = if null s then "" else last (words s)

-- | We still keep the monadic version for other potential uses
layoutName' :: String -> X String
layoutName' l = do
  LayoutNameCache cache <- XS.get
  case Map.lookup l cache of
    Just name -> return name
    Nothing -> do
      let name = layoutName l
      XS.put $ LayoutNameCache (Map.insert l name cache)
      return name

-- | Pure version for PP usage
layoutNamePure :: String -> String
layoutNamePure = layoutName

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
      ppLayout = xmobarColor white1 "" . layoutNamePure,
      ppOrder = \(ws : l : t : ex) -> [ws, l] ++ ex ++ [t],
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

  copiesPP copiesCurrentPP $
    topBarPP
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
  showWNameLogHook def
