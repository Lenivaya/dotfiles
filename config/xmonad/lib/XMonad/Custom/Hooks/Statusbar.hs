module XMonad.Custom.Hooks.Statusbar (
  barSpawner,
) where

import Control.Monad
import Data.Maybe
import Text.Printf
import XMonad
import XMonad.Custom.Hooks.Log
import XMonad.Hooks.StatusBar
import XMonad.Util.ClickableWorkspaces

barCommand :: Int -> String -> String
barCommand = printf command
  where
    command = "xmobar -x %d $XMONAD_CONFIG_DIR/xmobarrc/%s.hs"

xmobarTop :: Int -> StatusBarConfig
xmobarTop screen = statusBarPropTo "_XMONAD_LOG_1" (barCommand screen "top") topBarPP'

xmobarBot :: Int -> StatusBarConfig
xmobarBot screen = statusBarPropTo "_XMONAD_LOG_2" (barCommand screen "bot") (pure botBarPP)

barSpawner :: ScreenId -> X StatusBarConfig
barSpawner 0 = pure $ xmobarTop 0 <> xmobarBot 0 <> traySB -- two bars and tray on the main screen
barSpawner 1 = pure $ xmobarTop 1 <> xmobarBot 1 <> traySB -- two bars and tray on the main screen

traySB :: StatusBarConfig
traySB =
  statusBarGeneric
    "systemctl --user restart trayer.service"
    mempty

-- [Nov29 01:30] trayer[1334519]: segfault at 10000 ip 00007c24207fed85 sp 00007ffd0a655280 error 4 in libgdk-x11-2.0.so.0.2400.33[61d85,7c24207bb000+62000] likely on CPU 7 (co>
-- traySB :: StatusBarConfig
-- traySB =
--   statusBarGeneric
--     ( unwords
--         [ "trayer",
--           "-l",
--           "--SetDockType true --SetPartialStrut false",
--           "--edge top --align right",
--           "--widthtype request --expand true",
--           "--monitor primary",
--           "--tint 0x0B0806",
--           "--transparent true --alpha 10",
--           "--distancefrom top,right --distance 22,27",
--           "--height 25 --iconspacing 3 --padding 1 --margin 1"
--         ]
--     )
--     mempty
