module XMonad.Custom.Scratchpads
    ( scratchpads
    ) where

import           XMonad.Core
import           XMonad.Custom.Misc          as C
import           XMonad.ManageHook
import qualified XMonad.StackSet             as S
import           XMonad.Util.NamedScratchpad

spawnTerminalWith :: String -> String -> String
spawnTerminalWith t c = term applications ++ " -t " ++ t ++ " -e " ++ c

floatingNSP :: ManageHook
floatingNSP = customFloating $ S.RationalRect x y w h
    where
        x = (1 - w) / 2
        y = (1 - h) / 2
        w = 1 / 2
        h = 1 / 2.5

scratchpads :: [NamedScratchpad]
scratchpads =
    [ NS "console"
      (spawnTerminalWith "NSPConsole" "~/.xmonad/scripts/nsp-console.sh")
      (title =? "NSPConsole")
      floatingNSP
    , NS "volume"
      (spawnTerminalWith "NSPVolume" (C.mixer C.applications))
      (title =? "NSPVolume")
      floatingNSP
    , NS "music"
      (spawnTerminalWith "NSPMusic" (C.player C.applications))
      (title =? "NSPMusic")
      floatingNSP
    , NS "top"
      (spawnTerminalWith "NSPTop" (C.top C.applications))
      (title =? "NSPTop")
      floatingNSP
    ]
