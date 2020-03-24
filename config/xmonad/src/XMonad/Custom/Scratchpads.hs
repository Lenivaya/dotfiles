module XMonad.Custom.Scratchpads
    ( scratchpads
    ) where

import           XMonad.Core
import           XMonad.Custom.Misc          as C
import           XMonad.ManageHook
import qualified XMonad.StackSet             as S
import           XMonad.Util.NamedScratchpad

spawnTerminalWith :: String -> String -> String
spawnTerminalWith cl cm = term applications ++ " -c " ++ cl ++ " -e " ++ cm

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
      (spawnTerminalWith "NSPConsole" "~/.dotfiles/config/xmonad/scripts/nsp-console.sh")
      (className =? "NSPConsole")
      floatingNSP
    , NS "volume"
      (spawnTerminalWith "NSPVolume" (C.mixer C.applications))
      (className =? "NSPVolume")
      floatingNSP
    , NS "music"
      (C.player C.applications)
      (className =? "Spotify")
      floatingNSP
    , NS "top"
      (spawnTerminalWith "NSPTop" (C.top C.applications))
      (className =? "NSPTop")
      floatingNSP
    ]
