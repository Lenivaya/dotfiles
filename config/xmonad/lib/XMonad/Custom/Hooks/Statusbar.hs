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

barCommand :: String -> String
barCommand = printf command
  where
    command = "xmobar -x 0 $XMONAD_CONFIG_DIR/xmobarrc/%s.hs"

xmobarTop = statusBarPropTo "_XMONAD_LOG_1" (barCommand "top") topBarPP'
xmobarBot = statusBarPropTo "_XMONAD_LOG_2" (barCommand "bot") (pure botBarPP)

barSpawner :: ScreenId -> X StatusBarConfig
barSpawner 0 = pure $ xmobarTop <> xmobarBot <> traySB -- two bars and tray on the main screen
barSpawner _ = mempty -- nothing on the rest of the screens

traySB :: StatusBarConfig
traySB =
  statusBarGeneric
    ( unwords
        [ "trayer",
          "-l",
          "--SetDockType true --SetPartialStrut false",
          "--edge top --align right",
          "--widthtype request --expand true",
          "--monitor primary",
          "--tint 0x0B0806",
          "--transparent true --alpha 10",
          "--distancefrom top,right --distance 22,27",
          "--height 25 --iconspacing 3 --padding 1 --margin 1"
        ]
    )
    mempty

-- traySB :: StatusBarConfig
-- traySB =
--   statusBarGeneric
--     "systemctl --user restart tray.service"
--     mempty
