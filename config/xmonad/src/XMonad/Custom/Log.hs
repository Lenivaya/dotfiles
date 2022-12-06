{-# LANGUAGE LambdaCase #-}

module XMonad.Custom.Log
    ( logHook
    , topBarPP'
    , botBarPP
    ) where

import           Data.List                      ( isInfixOf )
import           System.IO
import           XMonad                  hiding ( logHook )
import           XMonad.Actions.CopyWindow
import           XMonad.Custom.Scratchpads
import           XMonad.Custom.Theme
import           XMonad.Hooks.CurrentWorkspaceOnTop
import           XMonad.Hooks.EwmhDesktops
import           XMonad.Hooks.RefocusLast
import           XMonad.Hooks.StatusBar.PP
import           XMonad.Util.ClickableWorkspaces
import           XMonad.Util.SpawnNamedPipe
import           XMonad.Util.WorkspaceCompare

-- layoutIcon :: String -> String
-- layoutIcon l | "BSP" `isInfixOf` l         = " <fn=1>\57654</fn>"
--              | "Circle" `isInfixOf` l      = " <fn=1>\57521</fn>"
--              | "Tall" `isInfixOf` l        = " <fn=1>\57346</fn>"
--              | "ThreeColMid" `isInfixOf` l = " <fn=1>\57377</fn>"
--              | "OneBig" `isInfixOf` l      = " <fn=1>\57377</fn>"
--              | otherwise                   = ""


layoutName :: String -> String
layoutName l | "BSP" `isInfixOf` l         = "BSP"
             | "Circle" `isInfixOf` l      = "Circle"
             | "Tall" `isInfixOf` l        = "Tall"
             | "ThreeColMid" `isInfixOf` l = "ThreeColMid"
             | "OneBig" `isInfixOf` l      = "OneBig"
             | "Monocle" `isInfixOf` l     = "Monocle"
             | "Grid" `isInfixOf` l        = "Grid"
             | otherwise                   = ""


topBarPP :: PP
topBarPP = def
    { ppCurrent         = xmobarColor white2 "" . xmobarFont 2 . wrap "=" "="
    , ppVisible         = xmobarColor white1 "" . wrap "~" "~"
    , ppHidden          = xmobarColor white1 "" . wrap "-" "-"
    , ppHiddenNoWindows = xmobarColor white1 "" . wrap "_" "_"
    , ppUrgent          = xmobarColor red2 "" . wrap "!" "!"
    , ppSep             = " / "
    , ppWsSep           = " "
    , ppTitle           = xmobarColor white1 "" . shorten 80
    , ppTitleSanitize   = xmobarStrip
    , ppLayout          = xmobarColor white1 "" . layoutName
    , ppOrder           = id
    , ppSort = (namedScratchpadFilterOutWorkspace .) <$> getSortByIndex
    , ppExtras          = []
    }

topBarPP' :: X PP
topBarPP' = do
    c <- wsContainingCopies
    let copiesCurrent ws
            | ws `elem` c
            = xmobarColor yellow2 "" . xmobarFont 2 . wrap "*" "=" $ ws
            | otherwise
            = xmobarColor white2 "" . xmobarFont 2 . wrap "=" "=" $ ws
    let copiesHidden ws
            | ws `elem` c = xmobarColor yellow1 "" . wrap "*" "-" $ ws
            | otherwise   = xmobarColor white1 "" . wrap "-" "-" $ ws
    let copiesUrgent ws
            | ws `elem` c = xmobarColor yellow2 "" . wrap "*" "!" $ ws
            | otherwise   = xmobarColor white2 "" . wrap "!" "!" $ ws
    clickablePP topBarPP { ppCurrent = copiesCurrent
                         , ppHidden  = copiesHidden
                         , ppUrgent  = copiesUrgent
                         }


botBarPP :: PP
botBarPP = topBarPP { ppCurrent         = const ""
                    , ppVisible         = const ""
                    , ppHidden          = const ""
                    , ppHiddenNoWindows = const ""
                    , ppUrgent          = const ""
                    , ppTitle           = const ""
                    , ppLayout          = const ""
                    }
logHook :: X ()
logHook = do
    refocusLastLogHook
    currentWorkspaceOnTop
