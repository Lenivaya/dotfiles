module XMonad.Custom.Startup
  ( startupHook
  ) where

import           Control.Monad
import           Data.Maybe
import           Graphics.Gloss.Interface.Environment
import           Text.Printf
import           XMonad                  hiding ( startupHook )
import           XMonad.Hooks.ManageDocks
import           XMonad.Hooks.SetWMName
import           XMonad.Util.SpawnNamedPipe
import           XMonad.Util.SpawnOnce

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
  where command = "xmobar $XMONAD_CONFIG_DIR/xmobarrc/%s.hs -p "

spawnXmobar :: X ()
spawnXmobar = do
  resolution <- liftIO getScreenSize
  spawnNamedPipe (barCommand resolution "top") "xmobarTop"
  spawnNamedPipe (barCommand resolution "bot") "xmobarBot"

startupHook :: X ()
startupHook = do
  spawnXmobar
  -- docksStartupHook
  spawn "betterlockscreen -w"
  setWMName "xmonad"
