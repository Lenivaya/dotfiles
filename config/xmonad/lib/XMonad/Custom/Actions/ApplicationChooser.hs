{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}

module XMonad.Custom.Actions.ApplicationChooser where

import Data.List
import Data.Map qualified as Map
import Data.Maybe
import GHC.Generics (Generic)
import XMonad
import XMonad.Custom.Prompt
import XMonad.Prompt
import XMonad.Prompt.Shell

data Application = Application
  { applicationCategory :: !AppCategory,
    applicationName :: !String,
    applicationCommand :: !String
  }
  deriving stock (Eq, Show, Generic)

newtype AppByName = AppByName
  { getApps :: [Application]
  }
  deriving stock (Generic)

instance XPrompt AppByName where
  showXPrompt (AppByName apps) =
    "Application (" <> categories <> "): "
    where
      categories = intercalate ", " . nub $ show . applicationCategory <$> apps

-- Define the AppGroup type class
class AppGroup a where
  getApplications :: a -> [Application]

data AppCategory
  = Browsers
  | Readers
  | SoundUtils
  | Editors
  deriving stock (Eq, Show, Generic)

instance AppGroup AppCategory where
  getApplications = \case
    Browsers -> myBrowsers
    Readers -> myReaders
    SoundUtils -> mySoundUtils
    Editors -> myEditors

-- Core application lists
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

-- Core selection function
selectAppByNameAndDo' :: XPConfig -> AppByName -> (String -> X ()) -> X ()
selectAppByNameAndDo' conf (AppByName apps) action = do
  cmds <- io getCommands
  let validApps = filter ((`elem` cmds) . applicationCommand) apps
      appMapping = Map.fromList $ map toNameCommandPair validApps

  mkXPrompt
    (AppByName apps)
    conf
    (aListCompFunc conf $ Map.toList appMapping)
    (maybe (pure ()) action . (`Map.lookup` appMapping))
  where
    toNameCommandPair app = (applicationName app, applicationCommand app)

-- Generic action creators
withCategory :: AppCategory -> (String -> X ()) -> XPConfig -> X ()
withCategory category action =
  selectAppByNameAndDo'
    `flip` AppByName (getApplications category)
    `flip` action

-- Fixed type signature and implementation
selectCategoryAndDo :: AppCategory -> XPConfig -> (String -> X ()) -> X ()
selectCategoryAndDo category conf action = withCategory category action conf

-- Common action creators
spawnFromCategory :: AppCategory -> XPConfig -> X ()
spawnFromCategory cat conf = selectCategoryAndDo cat conf spawn

terminalFromCategory :: AppCategory -> XPConfig -> X ()
terminalFromCategory cat conf = selectCategoryAndDo cat conf (spawn . ("$TERM -e " ++))

-- Convenience functions for common categories
selectEditorByNameAndDo :: XPConfig -> (String -> X ()) -> X ()
selectEditorByNameAndDo = selectCategoryAndDo Editors

selectBrowserByNameAndDo :: XPConfig -> (String -> X ()) -> X ()
selectBrowserByNameAndDo = selectCategoryAndDo Browsers

selectReaderByNameAndDo :: XPConfig -> (String -> X ()) -> X ()
selectReaderByNameAndDo = selectCategoryAndDo Readers

selectBrowserByNameAndSpawn :: XPConfig -> X ()
selectBrowserByNameAndSpawn conf = selectCategoryAndDo Browsers conf spawn

selectReaderByNameAndSpawn :: XPConfig -> X ()
selectReaderByNameAndSpawn conf = selectCategoryAndDo Readers conf spawn

selectEditorByNameAndSpawn :: XPConfig -> X ()
selectEditorByNameAndSpawn conf = selectCategoryAndDo Editors conf spawn
