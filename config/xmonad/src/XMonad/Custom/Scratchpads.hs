module XMonad.Custom.Scratchpads
  ( scratchpads
  , namedScratchpadFilterOutWorkspace
  ) where

import           XMonad.Core
import           XMonad.Custom.Misc            as C
import           XMonad.ManageHook
import qualified XMonad.StackSet               as S
import           XMonad.Util.NamedScratchpad
                                         hiding ( namedScratchpadFilterOutWorkspace
                                                )
import           XMonad.Util.WorkspaceCompare

spawnTerminalWith :: String -> String -> String
spawnTerminalWith cl cm =
  term applications ++ " --class " ++ cl ++ "," ++ cl ++ " -e " ++ cm

floatingNSP :: ManageHook
floatingNSP = customFloating $ S.RationalRect x y w h
 where
  x = (1 - w) / 2
  y = (1 - h) / 2
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
       (className =? "easyeffects")
       floatingNSP
  , NS "music" (C.player C.applications) (className =? "Spotify") nonFloating
  , NS "top"
       (spawnTerminalWith "NSPTop" (C.top C.applications))
       (className =? "NSPTop")
       floatingNSP
  , NS "discord" "Discord" (className =? "discord") floatingNSP
  ]


namedScratchpadFilterOutWorkspace = filterOutWs [scratchpadWorkspaceTag]
