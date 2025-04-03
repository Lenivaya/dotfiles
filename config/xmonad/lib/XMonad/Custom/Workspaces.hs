{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE RecordWildCards #-}

module XMonad.Custom.Workspaces where

import Data.Foldable
import XMonad hiding (workspaces)
import XMonad.Actions.DynamicProjects
import XMonad.Actions.WindowGo
import XMonad.Custom.Actions.ApplicationChooser
import XMonad.Custom.Actions.Keyboard
import XMonad.Custom.Misc qualified as C
import XMonad.Custom.Prompt
import XMonad.Custom.Scratchpads
import XMonad.Util.NamedScratchpad
import XMonad.Util.Run

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
wsNames =
  WorkspaceNames
    { generic = "GEN",
      code = "CODE",
      web = "WWW",
      wsread = "rEAD",
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
makeProject name = makeProject' name "~/"

makeProject' :: String -> String -> Maybe (X ()) -> Project
makeProject' name dir hook =
  Project
    { projectName = name,
      projectDirectory = dir,
      projectStartHook = hook
    }

-- Helper function to spawn terminal with a command
spawnTerminalWith :: String -> X ()
spawnTerminalWith cmd = spawn (C.term C.applications ++ " " ++ cmd)

spawnBrowserWithUrls :: [String] -> X ()
spawnBrowserWithUrls urls = spawn (C.browser C.applications ++ " --new-window " ++ unwords urls)

spawnBrowserWithUrl :: String -> X ()
spawnBrowserWithUrl url = spawn (C.browser C.applications ++ " --new-window " ++ url)

-- All projects are either long lived (workspaces) or dynamic workspaces for specific
-- use-cases, or dynamic projects used for starting something from use-case workspace `template`
projects :: [Project]
projects =
  [ makeProject (generic wsNames) Nothing,
    makeProject (template wsNames) $ Just $ do
      spawnTerminalWith "-e tmux"
      spawn (C.browser C.applications),
    makeProject (graphics wsNames) $ Just $ do
      spawn "gimp",
    makeProject (sound wsNames) $ Just $ do
      spawn (C.soundEffects C.applications)
      spawn "pavucontrol",
    makeProject (vm wsNames) $ Just $ do
      spawn (C.virtualMachinesManger C.applications),
    makeProject (write wsNames) $ Just $ do
      spawn "emacs_lets_write",
    makeProject (note wsNames) $ Just $ do
      namedScratchpadAction scratchpads "notes",
    -- spawn "obsidian",
    makeProject (code wsNames) $ Just $ do
      wrapKbdLayout $
        selectEditorByNameAndDo
          promptTheme
          spawn,
    makeProject (web wsNames) $ Just $ do
      wrapKbdLayout $
        selectBrowserByNameAndDo
          promptTheme
          spawn,
    makeProject (wsread wsNames) $ Just $ do
      wrapKbdLayout $
        selectReaderByNameAndDo
          promptTheme
          spawn,
    makeProject (sys wsNames) $ Just $ do
      spawn (C.term C.applications)
      spawn (C.term C.applications),
    makeProject (tmp wsNames) Nothing,
    makeProject (wsWRK wsNames) Nothing,
    makeProject (git wsNames) $ Just $ do spawn "gitbutler-tauri",
    makeProject (messages wsNames) $ Just $ do spawn "ayugram-desktop",
    makeProject "START" Nothing,
    makeProject "MON" $ Just $ do
      spawnTerminalWith "-e btop"
      spawnTerminalWith "-e htop",
    makeProject "AI" $ Just $ do
      spawnBrowserWithUrl "https://chat.openai.com"
      spawnBrowserWithUrls
        ["https://claude.ai", "https://www.perplexity.ai/", "https://copilot.microsoft.com", "https://chat.deepseek.com"],
    makeProject "GH" $ Just $ do
      spawnBrowserWithUrl "https://github.com"
      spawnBrowserWithUrls ["https://github.com/notifications", "https://github.com/pulls"],
    makeProject "MAIL" $ Just $ do
      spawnBrowserWithUrl "https://mail.google.com"
      spawnBrowserWithUrl "https://mail.proton.me",
    makeProject "TWR" $ Just $ do
      spawnBrowserWithUrl "https://x.com",
    makeProject "SPACE-ANALYZER" $ Just $ do
      spawnTerminalWith "--hold -e duf"
      spawnTerminalWith "-e dua i ~/",
    makeProject "WEATHER" $ Just $ do
      spawnTerminalWith "--hold -e curl wttr.in"
      spawnBrowserWithUrl "https://merrysky.net/",
    makeProject "DEV" $ Just $ do
      spawn "cursor"
      spawnTerminalWith "-e tmux"
      spawnBrowserWithUrl "https://devdocs.io",
    makeProject "NET" $ Just $ do
      spawnTerminalWith "--hold -e , speedtest-rs"
      spawnTerminalWith "-e , bmon",
    makeProject "LOGS" $ Just $ do
      spawnTerminalWith "--hold -e journalctl -f",
    makeProject "API" $ Just $ do
      spawn "postman",
    makeProject "LANG" $ Just $ do
      spawnBrowserWithUrls
        ["https://duolingo.com", "https://translate.google.com", "https://forvo.com"],
    makeProject "TRANSLATE" $ Just $ do
      spawnBrowserWithUrls
        ["https://translate.google.com", "https://deepl.com", "https://reverso.net"],
    makeProject "DOCKER" $ Just $ do
      spawnTerminalWith "-e lazydocker"
      spawnTerminalWith "-e oxker",
    makeProject "CRYPTO" $ Just $ do
      spawnTerminalWith "-e , cointop"
      spawnBrowserWithUrls ["https://x.com", "https://dropstab.com"],
    makeProject "REC" $ Just $ do
      spawn "obs",
    makeProject "WATCH" $ Just $ do
      spawnBrowserWithUrls
        [ "https://youtube.com",
          "https://www.youtube.com/playlist?list=WL",
          "https://www.youtube.com/feed/history"
        ],
    makeProject "UPWORK" $ Just $ do
      spawnBrowserWithUrls
        [ "https://www.upwork.com/",
          "https://www.upwork.com/nx/find-work/best-matches",
          "https://www.upwork.com/nx/plans/connects/history/"
        ]
      spawn "upwork",
    makeProject "DOTFILES" $ Just $ do
      spawnTerminalWith "--hold -e $DOTFILES"
      spawnTerminalWith "-e nvim $DOTFILES"
  ]
