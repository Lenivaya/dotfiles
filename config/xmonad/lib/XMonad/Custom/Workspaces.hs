{-# LANGUAGE RecordWildCards #-}

module XMonad.Custom.Workspaces where

import Data.Foldable
import XMonad hiding (workspaces)
import XMonad.Actions.DynamicProjects
import XMonad.Actions.SpawnOn
import XMonad.Actions.WindowGo
import XMonad.Custom.Actions.ApplicationChooser
import XMonad.Custom.Actions.Keyboard
import XMonad.Custom.Misc qualified as C
import XMonad.Custom.Prompt

data WorkspaceNames = WorkspaceNames
  { generic :: String,
    code :: String,
    web :: String,
    wsread :: String,
    sys :: String,
    tmp :: String,
    wsWRK :: String,
    template :: String,
    graphics :: String,
    sound :: String,
    vm :: String,
    write :: String,
    note :: String,
    git :: String,
    messages :: String
  }

wsNames :: WorkspaceNames
wsNames = WorkspaceNames
  { generic = "GEN",
    code = "Code",
    web = "WWW",
    wsread = "Read",
    sys = "SYS",
    tmp = "TMP",
    wsWRK = "WRK",
    template = "TEMPLATE",
    graphics = "GRAPH",
    sound = "SOUND",
    vm = "VM",
    write = "WRITE",
    note = "NOTE",
    git = "GIT",
    messages = "MSG"
  }

workspaces :: [String]
workspaces = [generic wsNames, sys wsNames, tmp wsNames, wsWRK wsNames, code wsNames, web wsNames]

-- Helper function to create a basic project
makeProject :: String -> Maybe (X ()) -> Project
makeProject name hook = makeProject' name "~/" hook

makeProject' :: String -> String -> Maybe (X ()) -> Project
makeProject' name dir hook = Project
  { projectName = name,
    projectDirectory = dir,
    projectStartHook = hook
  }

-- Helper function to spawn terminal with a command
spawnTerminalWith :: String -> String -> X ()
spawnTerminalWith ws cmd = spawnOn ws (C.term C.applications ++ " " ++ cmd)

spawnBrowserWithUrls :: String -> [String] -> X ()
spawnBrowserWithUrls ws urls = spawnOn ws (C.browser C.applications ++ " " ++ unwords urls)

spawnBrowserWithUrl :: String -> String -> X ()
spawnBrowserWithUrl ws url = spawnOn ws (C.browser C.applications ++ " --new-window " ++ url)

-- All projects are either long lived (workspaces) or dynamic workspaces for specific
-- use-cases, or dynamic projects used for starting something from use-case workspace `template`
projects :: [Project]
projects =
  [ makeProject (generic wsNames) Nothing,
    makeProject (template wsNames) $ Just $ do
      spawnTerminalWith (template wsNames) "-e tmux"
      spawnOn (template wsNames) (C.browser C.applications),
    makeProject (graphics wsNames) $ Just $ do
      spawnOn (graphics wsNames) "gimp",
    makeProject (sound wsNames) $ Just $ do
      spawnOn (sound wsNames) (C.soundEffects C.applications)
      spawnOn (sound wsNames) "pavucontrol",
    makeProject (vm wsNames) $ Just $ do
      spawnOn (vm wsNames) (C.virtualMachinesManger C.applications),
    makeProject (write wsNames) $ Just $ do
      spawnOn (write wsNames) "emacs_lets_write",
    makeProject (note wsNames) $ Just $ do
      spawnOn (note wsNames) "obsidian",
    makeProject (code wsNames) $ Just raiseEditor,
    makeProject (web wsNames) $ Just $ do
      wrapKbdLayout $
        selectBrowserByNameAndDo
          promptTheme
          (spawnOn (web wsNames)),
    makeProject (wsread wsNames) $ Just $ do
      wrapKbdLayout $
        selectReaderByNameAndDo
          promptTheme
          (spawnOn (wsread wsNames)),
    makeProject (sys wsNames) $ Just $ do
      spawnOn (sys wsNames) (C.term C.applications)
      spawnOn (sys wsNames) (C.term C.applications),
    makeProject (tmp wsNames) Nothing,
    makeProject (wsWRK wsNames) Nothing,
    makeProject (git wsNames) $ Just $ do spawnOn (git wsNames) "gitbutler-tauri",
    makeProject (messages wsNames) $ Just $ do spawnOn (messages wsNames) "telegram-desktop",
    makeProject "MON" $ Just $ do
      spawnTerminalWith "MON" "-e btop"
      spawnTerminalWith "MON" "-e htop",
    makeProject "AI" $ Just $ do
      spawnBrowserWithUrl "AI" "https://chat.openai.com"
      spawnBrowserWithUrl "AI" "https://claude.ai",
    makeProject "GH" $ Just $ do
      spawnBrowserWithUrl "GH" "https://github.com"
      spawnBrowserWithUrls "GH" ["https://github.com/notifications", "https://github.com/pulls"],
    makeProject "MAIL" $ Just $ do
      spawnBrowserWithUrl "MAIL" "https://mail.google.com"
      spawnBrowserWithUrl "MAIL" "https://mail.proton.me",
    makeProject "TWR" $ Just $ do
      spawnBrowserWithUrl "TWR" "https://x.com",
    makeProject "SPACE-ANALYZER" $ Just $ do
      spawnTerminalWith "SPACE-ANALYZER" "--hold -e duf"
      spawnTerminalWith "SPACE-ANALYZER" "-e dua i ~/",
    makeProject "WEATHER" $ Just $ do
      spawnTerminalWith "WEATHER" "--hold -e curl wttr.in"
      spawnBrowserWithUrl "WEATHER" "https://merrysky.net/",
    makeProject "DEV" $ Just $ do
      spawnOn "DEV" "cursor"
      spawnTerminalWith "DEV" "-e tmux"
      spawnBrowserWithUrl "DEV" "https://devdocs.io",
    makeProject "NET" $ Just $ do
      spawnTerminalWith "NET" "--hold -e , speedtest-rs"
      spawnTerminalWith "NET" "-e , bmon",
    makeProject "LOGS" $ Just $ do
      spawnTerminalWith "LOGS" "--hold -e journalctl -f",
    makeProject "API" $ Just $ do
      spawnOn "API" "postman",
    makeProject "LANG" $ Just $ do
      spawnBrowserWithUrls "LANG" ["https://duolingo.com", "https://translate.google.com", "https://forvo.com"],
    makeProject "TRANSLATE" $ Just $ do
      spawnBrowserWithUrls "TRANSLATE" ["https://translate.google.com", "https://deepl.com", "https://reverso.net"],
    makeProject "DOCKER" $ Just $ do
      spawnTerminalWith "DOCKER" "-e lazydocker"
      spawnTerminalWith "DOCKER" "-e oxker",
    makeProject "CRYPTO" $ Just $ do
      spawnTerminalWith "CRYPTO" "-e , cointop"
      spawnBrowserWithUrls "CRYPTO" ["https://x.com", "https://dropstab.com", "https://binance.com"],
    makeProject "REC" $ Just $ do
      spawnOn "REC" "obs",
    makeProject "WATCH" $ Just $ do
      spawnBrowserWithUrls "WATCH" ["https://youtube.com", "https://www.youtube.com/playlist?list=WL"],
    makeProject "UPWORK" $ Just $ do
      spawnBrowserWithUrls "UPWORK" ["https://www.upwork.com/", "https://www.upwork.com/nx/find-work/best-matches", "https://www.upwork.com/nx/plans/connects/history/"]
      spawnOn "UPWORK" "upwork"
  ]
