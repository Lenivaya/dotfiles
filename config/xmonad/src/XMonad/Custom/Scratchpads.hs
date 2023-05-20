module XMonad.Custom.Scratchpads
  ( scratchpads
  , namedScratchpadFilterOutWorkspace
  ) where

import           XMonad.Core
import           XMonad.Custom.ManageHelpers    ( centerFloat )
import           XMonad.Custom.Misc            as C
import           XMonad.ManageHook
import qualified XMonad.StackSet               as S
import           XMonad.Util.NamedScratchpad
                                         hiding ( namedScratchpadFilterOutWorkspace
                                                )
import           XMonad.Util.WorkspaceCompare



spawnTerminalWith :: String -> String -> String
spawnTerminalWith className command = unwords $ terminal : options
 where
  terminal = term applications
  options  = ["--class", class', "-e", command]
  class'   = className ++ "," ++ className


floatingNSP :: ManageHook
floatingNSP = centerFloat w h
 where
  w = 1 / 2
  h = 1 / 2.5

scratchpads :: [NamedScratchpad]
scratchpads =
  [ NS
    "console"
    (spawnTerminalWith "NSPConsole" "$XMONAD_CONFIG_DIR/scripts/nsp-console.sh")
    (className =? "NSPConsole")
    floatingNSP
  , NS "volume"
       (spawnTerminalWith "NSPVolume" (C.mixer C.applications))
       (className =? "NSPVolume")
       floatingNSP
  , NS "soundEffects"
       (C.soundEffects C.applications)
       (appName =? "easyeffects")
       (centerFloat 0.6 0.6)
  , NS "music"
       (C.player C.applications)
       (className =? "Spotify")
       doFullCenterFloat
  , NS "top"
       (spawnTerminalWith "NSPTop" (C.top C.applications))
       (className =? "NSPTop")
       floatingNSP
  , NS "discord" "Discord" (className =? "discord") doFullCenterFloat
  ]
  where doFullCenterFloat = centerFloat 0.7 0.7


namedScratchpadFilterOutWorkspace = filterOutWs [scratchpadWorkspaceTag]
