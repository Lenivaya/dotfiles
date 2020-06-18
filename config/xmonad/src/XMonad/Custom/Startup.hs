module XMonad.Custom.Startup
  ( startupHook
  )
where

import           Control.Monad
import           Text.Printf
import           Data.Maybe
import           System.IO.Unsafe
import           Graphics.Gloss.Interface.Environment
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

addEWMHFullscreen :: X ()
addEWMHFullscreen = do
  s <- mapM getAtom atomsToFullscreen
  mapM_ addNETSupported s


getResolution :: (Int, Int)
getResolution = unsafePerformIO $ do
  resolution <- getScreenSize
  return resolution

percentFromNumber :: Int -> Float -> Int
percentFromNumber n p = round (realToFrac n * realToFrac (p / 100))

generateBarPosition :: String -> String
generateBarPosition bar | bar == "top" = printf position topBarYpos width
                        | bar == "bot" = printf position botBarYpos width
 where
  position   = "'Static { xpos = 12 , ypos = %d , width = %d , height = 24}'"
  (w, h)     = getResolution
  width      = w - 24
  botBarYpos = (h - (percentFromNumber h 1.0)) - 24
  topBarYpos = percentFromNumber h 1.0

topBarCommand = "xmobar ~/.dotfiles/config/xmonad/xmobarrc/top.hs -p "
  ++ generateBarPosition "top"
botBarCommand = "xmobar ~/.dotfiles/config/xmonad/xmobarrc/bot.hs -p "
  ++ generateBarPosition "bot"

startupHook :: X ()
startupHook = do
  spawnNamedPipe topBarCommand "xmobarTop"
  spawnNamedPipe botBarCommand "xmobarBot"
  docksStartupHook
  addEWMHFullscreen
  -- spawnOnce "xsetroot -cursor_name left_ptr"
  spawn "betterlockscreen -w"
  setWMName "xmonad"
