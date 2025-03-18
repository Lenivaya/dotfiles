{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module XMonad.Custom.Actions.ApplicationChooser where

import Data.List (intercalate, nub)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import GHC.Generics (Generic)
import XMonad
import XMonad.Custom.Prompt
import XMonad.Prompt
import XMonad.Prompt.Shell

data AppCategory
  = Browsers
  | Readers
  | SoundUtils
  | Editors
  deriving stock (Eq, Show, Ord, Generic)

data Application = Application
  { applicationCategory :: !AppCategory,
    applicationName :: !String,
    applicationCommand :: !String
  }
  deriving stock (Eq, Show, Generic)

-- A more efficient structure for storing application data
data AppRegistry = AppRegistry
  { appsByCategory :: !(Map.Map AppCategory [Application]),
    appNameToCommand :: !(Map.Map String String)
  }
  deriving stock (Generic)

-- Initialize the global registry (computed only once)
appRegistry :: AppRegistry
appRegistry = initRegistry allApplications
  where
    allApplications = concat [myBrowsers, myReaders, mySoundUtils, myEditors]

    initRegistry apps =
      AppRegistry
        { appsByCategory = foldr addToCategory Map.empty apps,
          appNameToCommand =
            Map.fromList [(applicationName app, applicationCommand app) | app <- apps]
        }

    addToCategory app = Map.alter (Just . maybe [app] (app :)) (applicationCategory app)

-- Display type for the prompt
newtype AppPrompt = AppPrompt AppCategory
  deriving stock (Generic)

instance XPrompt AppPrompt where
  showXPrompt (AppPrompt category) = "Application (" <> show category <> "): "

-- Core application lists - kept for organization and readability
myBrowsers, myReaders, mySoundUtils, myEditors :: [Application]
myBrowsers =
  [ Application Browsers "Firefox" "firefox",
    Application Browsers "Firefox Nightly" "firefox-nightly",
    Application Browsers "Chromium" "chromium",
    Application Browsers "Google Chrome" "google-chrome-stable",
    Application Browsers "Google Chrome Unstable" "google-chrome-unstable",
    Application Browsers "Brave" "brave",
    Application Browsers "Qutebrowser" "qutebrowser"
  ]
myReaders =
  [ Application Readers "Zathura" "zathura",
    Application Readers "Readest" "readest",
    Application Readers "Evince" "evince",
    Application Readers "Papers" "papers"
  ]
mySoundUtils =
  [ Application SoundUtils "Easyeffects" "easyeffects",
    Application SoundUtils "Pavucontrol" "pavucontrol"
  ]
myEditors =
  [ Application Editors "vim" "nvimedit",
    Application Editors "VSCode" "code",
    Application Editors "Cursor" "cursor",
    Application Editors "Emacs" "emacseditor",
    Application Editors "Zed" "zeditor"
  ]

-- Get applications by category from the registry
getApplicationsByCategory :: AppCategory -> [Application]
getApplicationsByCategory category =
  fromMaybe [] $ Map.lookup category $ appsByCategory appRegistry

-- Core selection function - optimized to use the registry
selectAppByCategory :: AppCategory -> XPConfig -> (String -> X ()) -> X ()
selectAppByCategory category conf action = do
  cmds <- io getCommands
  let apps = getApplicationsByCategory category
      -- Filter only valid applications
      validAppNames =
        Map.fromList
          [ (applicationName app, applicationCommand app)
            | app <- apps,
              applicationCommand app `elem` cmds
          ]

  mkXPrompt
    (AppPrompt category)
    conf
    (aListCompFunc conf $ Map.toList validAppNames)
    (maybe (pure ()) action . (`Map.lookup` validAppNames))

-- Common action creators with simpler implementation
spawnFromCategory :: AppCategory -> XPConfig -> X ()
spawnFromCategory category conf = selectAppByCategory category conf spawn

terminalFromCategory :: AppCategory -> XPConfig -> X ()
terminalFromCategory category conf = selectAppByCategory category conf (spawn . ("$TERM -e " ++))

-- Convenience functions for common categories (simplified)
selectBrowserAndDo :: XPConfig -> (String -> X ()) -> X ()
selectBrowserAndDo = selectAppByCategory Browsers

selectReaderAndDo :: XPConfig -> (String -> X ()) -> X ()
selectReaderAndDo = selectAppByCategory Readers

selectEditorAndDo :: XPConfig -> (String -> X ()) -> X ()
selectEditorAndDo = selectAppByCategory Editors

selectSoundUtilAndDo :: XPConfig -> (String -> X ()) -> X ()
selectSoundUtilAndDo = selectAppByCategory SoundUtils

-- For backward compatibility
selectBrowserByNameAndDo :: XPConfig -> (String -> X ()) -> X ()
selectBrowserByNameAndDo = selectBrowserAndDo

selectReaderByNameAndDo :: XPConfig -> (String -> X ()) -> X ()
selectReaderByNameAndDo = selectReaderAndDo

selectEditorByNameAndDo :: XPConfig -> (String -> X ()) -> X ()
selectEditorByNameAndDo = selectEditorAndDo

selectBrowserByNameAndSpawn :: XPConfig -> X ()
selectBrowserByNameAndSpawn conf = selectBrowserAndDo conf spawn

selectReaderByNameAndSpawn :: XPConfig -> X ()
selectReaderByNameAndSpawn conf = selectReaderAndDo conf spawn

selectEditorByNameAndSpawn :: XPConfig -> X ()
selectEditorByNameAndSpawn conf = selectEditorAndDo conf spawn
