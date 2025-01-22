module XMonad.Custom.Scratchpads (
  scratchpads,
  namedScratchpadFilterOutWorkspace,
) where

import XMonad.Core
import XMonad.Custom.Manage.ManageHelpers (centerFloat)
import XMonad.Custom.Misc as C
import XMonad.ManageHook
import XMonad.StackSet qualified as S
import XMonad.Util.NamedScratchpad hiding (
  namedScratchpadFilterOutWorkspace,
 )
import XMonad.Util.WorkspaceCompare
import XMonad.Custom.Manage.ManageHelpers

spawnTerminalWith :: String -> String -> String
spawnTerminalWith className command = unwords $ terminal : options
  where
    terminal = term applications
    options = ["--class", className, "-e", command]
    -- class' = className ++ "," ++ className

floatingNSP :: ManageHook
floatingNSP = centerFloat w h
  where
    w = 1 / 2
    h = 1 / 1.5
    -- h = 1 / 2.5

scratchpads :: [NamedScratchpad]
scratchpads =
  [ NS
      "console"
      (spawnTerminalWith "NSPConsole" "$XMONAD_CONFIG_DIR/scripts/nsp-console.sh")
      (className =? "NSPConsole")
      doFullCenterFloat,
    NS
      "volume"
      (spawnTerminalWith "NSPVolume" (C.mixer C.applications))
      (className =? "NSPVolume")
      floatingNSP,
    NS
      "soundEffects"
      (C.soundEffects C.applications)
      (appName =? "easyeffects")
      (centerFloat 0.6 0.6),
    NS
      "music"
      (C.player C.applications)
      (className =? "Spotify")
      doFullCenterFloat,
    NS
      "top"
      (spawnTerminalWith "NSPTop" (C.top C.applications))
      (className =? "NSPTop")
      doFullCenterFloat,
    NS
      "discord"
      "legcord"
      (className =? "legcord")
      doFullCenterFloat,
    NS
      "cpupower"
      "cpupower-gui"
      (className =? "Cpupower-gui")
      (centerFloat 0.7 0.7),
    -- NS
    --   "telegram"
    --   "telegram-desktop"
    --   (className =? "TelegramDesktop")
    --   doFullCenterFloat,
    NS
      "reader"
      "foliate"
      (className =? "com.github.johnfactotum.Foliate")
      doFullCenterFloat,
    NS
      "notes"
      "obsidian"
      (className =? "obsidian")
      doFullCenterFloat,
   NS
      "logs"
      (spawnTerminalWith "NSPLogs" "--hold -e journalctl -f")
      (className =? "NSPLogs")
      doFullCenterFloat,
   NS
      "calculator"
      (spawnTerminalWith "NSPCalc" "qalc")
      (className =? "NSPCalc")
      (centerFloat 0.4 0.4),
   NS
      "calendar"
      (spawnTerminalWith "NSPCalendar" "khal interactive")
      (className =? "NSPCalendar")
      (centerFloat 0.4 0.4),
   NS
      "translate"
      (spawnTerminalWith "NSPTrans" "trans :ru -I")
      (className =? "NSPTrans")
      (centerFloat 0.5 0.5),
   NS
      "bluetooth"
      (spawnTerminalWith "NSPTrans" "bluetuith")
      (className =? "NSPBluetooth")
      (centerFloat 0.5 0.5)
  ]
  where
    doFullCenterFloat = centerFloat 0.85 0.85

namedScratchpadFilterOutWorkspace = filterOutWs [scratchpadWorkspaceTag]
