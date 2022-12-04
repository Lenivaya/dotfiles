module XMonad.Custom.Event
    ( handleEventHook
    ) where

import qualified Data.Map.Strict               as M
import           Data.Monoid
import           XMonad                  hiding ( handleEventHook
                                                , manageHook
                                                )
import           XMonad.Custom.Scratchpads
import           XMonad.Hooks.DynamicProperty
import           XMonad.Hooks.EwmhDesktops
import           XMonad.Hooks.ManageDocks
import           XMonad.Hooks.PerWindowKbdLayout
import           XMonad.Hooks.RefocusLast
import           XMonad.Hooks.WindowSwallowing
import qualified XMonad.Util.Hacks             as Hacks
import           XMonad.Util.Loggers.NamedScratchpad

import           XMonad.Custom.Manage           ( manageHook )
import           XMonad.Operations

-- Keeps last focused window
myPred = refocusingIsActive <||> isFloat

-- restartEventHook e@ClientMessageEvent { ev_message_type = mt } = do
--   a <- getAtom "XMONAD_RESTART"
--   if mt == a
--     then restart "xmonad" True >> return (All True)
--     else return $ All True
-- restartEventHook _ = return $ All True

handleEventHook :: Event -> X All
handleEventHook = mconcat
    [ swallowEventHook (className =? "Alacritty") (return True)
    , nspTrackHook scratchpads
  -- , docksEventHook
    , Hacks.windowedFullscreenFixEventHook
    , refocusLastWhen myPred
    , perWindowKbdLayout
  -- , restartEventHook
  -- , dynamicTitle manageHook
    ]
