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

percentFromNumber :: Int -> Float -> Int
percentFromNumber n p = round (realToFrac n * realToFrac (p / 100))

generateBarPosition :: (Int, Int) -> String -> String
generateBarPosition (w, h) pos | pos == "top" = printf position topBarYpos width
                               | pos == "bot" = printf position botBarYpos width
  where
    position   = "'Static { xpos = 12 , ypos = %d , width = %d , height = 32}'"
    width      = w - 32
    botBarYpos = (h - percentFromNumber h 1.0) - 32
    topBarYpos = percentFromNumber h 1.0

barCommand :: (Int, Int) -> String -> String
barCommand res pos = printf command pos ++ generateBarPosition res pos
    where command = "xmobar -x 0 $XMONAD_CONFIG_DIR/xmobarrc/%s.hs -p "

xmobarTop res =
    statusBarPropTo "_XMONAD_LOG_1" (barCommand res "top") topBarPP'
xmobarBot res =
    statusBarPropTo "_XMONAD_LOG_2" (barCommand res "bot") (pure botBarPP)

barSpawner :: (Int, Int) -> ScreenId -> IO StatusBarConfig
barSpawner res 0 = pure $ xmobarTop res <> xmobarBot res -- two bars on the main screen
-- barSpawner 1 = pure $ xmobar1
barSpawner _   _ = mempty -- nothing on the rest of the screens
