module XMonad.Custom.Event
    ( handleEventHook
    ) where

import           Data.Monoid
import           XMonad                              hiding (handleEventHook)
import           XMonad.Custom.Scratchpads
import           XMonad.Hooks.EwmhDesktops
import           XMonad.Hooks.ManageDocks
import           XMonad.Util.Loggers.NamedScratchpad
import           XMonad.Hooks.RefocusLast
import           qualified Data.Map.Strict as M

-- Keeps last focused window
myPred = refocusingIsActive <||> isFloat
refocusLastKeys cnf
 = M.fromList
 $ ((modMask cnf              , xK_a), toggleFocus)
 : ((modMask cnf .|. shiftMask, xK_a), swapWithLast)
 : ((modMask cnf              , xK_b), toggleRefocusing)
 : [ ( (modMask cnf .|. shiftMask, n)
     , windows =<< shiftRLWhen myPred wksp
     )
   | (n, wksp) <- zip [xK_1..xK_9] (workspaces cnf)
   ]

handleEventHook :: Event -> X All
handleEventHook = mconcat [ nspTrackHook scratchpads
                          , docksEventHook
                          , fullscreenEventHook
                          , refocusLastWhen myPred
                          ]
