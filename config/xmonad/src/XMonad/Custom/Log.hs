{-# LANGUAGE LambdaCase #-}

module XMonad.Custom.Log
  ( logHook
  )
where

import           System.IO
import           Data.List                      ( isInfixOf )
import           XMonad                  hiding ( logHook )
import           XMonad.Actions.CopyWindow
import           XMonad.Custom.Theme
import           XMonad.Hooks.CurrentWorkspaceOnTop
import           XMonad.Hooks.DynamicLog
import           XMonad.Hooks.EwmhDesktops
import           XMonad.Util.NamedScratchpad
import           XMonad.Util.SpawnNamedPipe
import           XMonad.Util.WorkspaceCompare

xmobarFont :: Int -> String -> String
xmobarFont f = wrap (concat ["<fn=", show f, ">"]) "</fn>"

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
  , ppLayout          = xmobarColor white1 ""
                          . (\layout -> case isInfixOf "BSP" layout of
                              True  -> " <fn=1>\57654</fn>"
                              False -> case isInfixOf "Circle" layout of
                                True  -> " <fn=1>\57521</fn>"
                                False -> case isInfixOf "Tall" layout of
                                  True  -> " <fn=1>\57346</fn>"
                                  False -> " <fn=1>\57377</fn>" -- ThreeColMid
                            )
  , ppOrder           = id
  , ppSort            = (namedScratchpadFilterOutWorkspace .) <$> getSortByIndex
  , ppExtras          = []
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

safePrintToPipe :: Maybe Handle -> String -> IO ()
safePrintToPipe = maybe (\_ -> return ()) hPutStrLn

logHook :: X ()
logHook = do
  currentWorkspaceOnTop
  ewmhDesktopsLogHook
  t <- getNamedPipe "xmobarTop"
  b <- getNamedPipe "xmobarBot"
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
  dynamicLogWithPP $ topBarPP { ppCurrent = copiesCurrent
                              , ppHidden  = copiesHidden
                              , ppUrgent  = copiesUrgent
                              , ppOutput  = safePrintToPipe t
                              }
  dynamicLogWithPP $ botBarPP { ppOutput = safePrintToPipe b }
