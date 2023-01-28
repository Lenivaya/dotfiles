module XMonad.Custom.Statusbar
  ( barSpawner
  ) where

import           Control.Monad
import           Data.Maybe
import           Text.Printf
import           XMonad
import           XMonad.Custom.Log
import           XMonad.Hooks.StatusBar
import           XMonad.Util.ClickableWorkspaces

barCommand :: String -> String
barCommand pos = printf command pos
  where command = "xmobar -x 0 $XMONAD_CONFIG_DIR/xmobarrc/%s.hs"

xmobarTop = statusBarPropTo "_XMONAD_LOG_1" (barCommand "top") topBarPP'
xmobarBot = statusBarPropTo "_XMONAD_LOG_2" (barCommand "bot") (pure botBarPP)

barSpawner :: ScreenId -> IO StatusBarConfig
barSpawner 0 = pure $ xmobarTop <> xmobarBot  -- two bars on the main screen
barSpawner _ = mempty -- nothing on the rest of the screens
