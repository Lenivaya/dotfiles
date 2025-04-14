{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE RecordWildCards #-}

module XMonad.Custom.Workspaces where

import Data.List (find)
import Data.Maybe (fromMaybe)
import XMonad hiding (workspaces)
import XMonad.Actions.DynamicProjects
import XMonad.Actions.WindowGo
import XMonad.Custom.Actions.ApplicationChooser
import XMonad.Custom.Actions.DoActions
import XMonad.Custom.Actions.Keyboard
import XMonad.Custom.Misc qualified as C
import XMonad.Custom.Prompt
import XMonad.Custom.Scratchpads
import XMonad.Util.NamedScratchpad

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

{-| Helper function to find and execute an action by name
Returns a no-op if the action isn't found
-}
doActionByName :: String -> X ()
doActionByName name = fromMaybe (return ()) $ fmap doAction (findActionByName name)

-- | Helper function to spawn an application by name using the ApplicationChooser
spawnAppByName :: AppCategory -> String -> X ()
spawnAppByName category appName = do
  let apps = getApplicationsByCategory category
      maybeApp = find (\app -> applicationName app == appName) apps
  case maybeApp of
    Just app -> spawn (applicationCommand app)
    Nothing -> return ()

-- All projects are either long lived (workspaces) or dynamic workspaces for specific
-- use-cases, or dynamic projects used for starting something from use-case workspace `template`
projects :: [Project]
projects =
  [ makeProject (generic wsNames) Nothing,
    makeProject (template wsNames) $ Just $ do
      doActionByName "Terminal with Tmux"
      doActionByName "Browser",
    makeProject (graphics wsNames) $ Just $ do
      spawn "gimp",
    makeProject (sound wsNames) $ Just $ do
      doActionByName "Sound Effects"
      spawn "pavucontrol",
    makeProject (vm wsNames) $ Just $ do
      spawn (C.virtualMachinesManger C.applications),
    makeProject (write wsNames) $ Just $ do
      spawn "emacs_lets_write",
    makeProject (note wsNames) $ Just $ do
      doActionByName "Notes",
    makeProject (code wsNames) $ Just $ do
      wrapKbdLayout $ selectEditorByNameAndDo promptTheme spawn,
    makeProject (web wsNames) $ Just $ do
      wrapKbdLayout $ selectBrowserByNameAndDo promptTheme spawn,
    makeProject (wsread wsNames) $ Just $ do
      wrapKbdLayout $ selectReaderByNameAndDo promptTheme spawn,
    makeProject (sys wsNames) $ Just $ do
      doActionByName "Terminal"
      doActionByName "Terminal",
    makeProject (tmp wsNames) Nothing,
    makeProject (wsWRK wsNames) Nothing,
    makeProject (git wsNames) $ Just $ doActionByName "GitButler",
    makeProject (messages wsNames) $ Just $ spawn "ayugram-desktop",
    makeProject "START" Nothing,
    makeProject "MON" $ Just $ doActionByName "System Monitors",
    makeProject "AI" $ Just $ doActionByName "AI Assistants",
    makeProject "GH" $ Just $ doActionByName "GitHub",
    makeProject "MAIL" $ Just $ doActionByName "Mail",
    makeProject "TWR" $ Just $ doActionByName "Social Media",
    makeProject "SPACE-ANALYZER" $ Just $ doActionByName "Disk Space Analyzer",
    makeProject "WEATHER" $ Just $ doActionByName "Weather",
    makeProject "DEV" $ Just $ do
      spawnAppByName Editors "Cursor"
      doActionByName "Terminal with Tmux"
      doActionByName "Browser",
    makeProject "NET" $ Just $ doActionByName "Network Monitors",
    makeProject "LOGS" $ Just $ doActionByName "System Logs",
    makeProject "API" $ Just $ doActionByName "API Client",
    makeProject "LANG" $ Just $ doActionByName "Language Learning",
    makeProject "TRANSLATE" $ Just $ doActionByName "Translation Tools",
    makeProject "DOCKER" $ Just $ doActionByName "Docker Tools",
    makeProject "CRYPTO" $ Just $ doActionByName "Cryptocurrency Tools",
    makeProject "REC" $ Just $ doActionByName "Screen Recorder",
    makeProject "WATCH" $ Just $ doActionByName "Video Streaming",
    makeProject "UPWORK" $ Just $ doActionByName "Upwork",
    makeProject "DOTS" $ Just $ doActionByName "Dotfiles"
  ]
