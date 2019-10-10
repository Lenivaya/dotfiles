module XMonad.Custom.Startup
    ( startupHook
    ) where

import           Control.Monad
import           Data.Maybe
import           XMonad                     hiding (startupHook)
import           XMonad.Hooks.ManageDocks
import           XMonad.Hooks.SetWMName
import           XMonad.Util.Cursor
import           XMonad.Util.SpawnNamedPipe

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
        when (fromIntegral x `notElem` p) $ changeProperty32 d r n a propModeAppend [fromIntegral x]

addEWMHFullscreen :: X ()
addEWMHFullscreen = do
    s <- mapM getAtom atomsToFullscreen
    mapM_ addNETSupported s

startupHook :: X ()
startupHook = do
    spawnNamedPipe "xmobar ~/.xmonad/xmobarrc/top.hs" "xmobarTop"
    spawnNamedPipe "xmobar ~/.xmonad/xmobarrc/bot.hs" "xmobarBot"
    docksStartupHook
    addEWMHFullscreen
    setDefaultCursor xC_left_ptr
    setWMName "xmonad"
    spawn "sxhkd -c ~/.config/sxhkd/xmonad/sxhkdrc"
