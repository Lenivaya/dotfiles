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

atomsToFullscreen :: [String]
atomsToFullscreen =
  [ "_NET_ACTIVE_WINDOW"
  , "_NET_CLIENT_LIST"
  , "_NET_CLIENT_LIST_STACKING"
  , "_NET_DESKTOP_NAMES"
  , "_NET_WM_DESKTOP"
  , "_NET_WM_STATE"
  , "_NET_WM_STATE_FULLSCREEN"
  , "_NET_WM_STATE_HIDDEN"
  , "_NET_WM_STRUT"
  ]

addNETSupported :: Atom -> X ()
addNETSupported x = withDisplay $ \d -> do
  r <- asks theRoot
  n <- getAtom "_NET_SUPPORTED"
  a <- getAtom "ATOM"
  liftIO $ do
    p <- join . maybeToList <$> getWindowProperty32 d n r
    when (fromIntegral x `notElem` p)
      $ changeProperty32 d r n a propModeAppend [fromIntegral x]

-- TODO replace this with ewmhFullscreen when new version of xmonad-contrib arrives
addEWMHFullscreen :: X ()
addEWMHFullscreen = do
  s <- mapM getAtom atomsToFullscreen
  mapM_ addNETSupported s

percentFromNumber :: Int -> Float -> Int
percentFromNumber n p = round (realToFrac n * realToFrac (p / 100))

generateBarPosition :: (Int, Int) -> String -> String
generateBarPosition (w, h) pos | pos == "top" = printf position topBarYpos width
                               | pos == "bot" = printf position botBarYpos width
 where
  position   = "'Static { xpos = 12 , ypos = %d , width = %d , height = 24}'"
  width      = w - 24
  botBarYpos = (h - percentFromNumber h 1.0) - 24
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
  docksStartupHook
  addEWMHFullscreen
  spawn "betterlockscreen -w"
  setWMName "xmonad"
