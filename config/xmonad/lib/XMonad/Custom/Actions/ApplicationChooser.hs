module XMonad.Custom.Actions.ApplicationChooser where

import Data.List
import Data.Maybe
import XMonad
import XMonad.Custom.Prompt
import XMonad.Prompt
import XMonad.Prompt.Shell

data Application = Application
  { applicationCategory :: String
  , applicationName :: String
  , applicationCommand :: String
  }

newtype AppByName = AppByName [Application]

instance XPrompt AppByName where
  showXPrompt (AppByName apps) = prompt
    where
      prompt = mconcat ["Application (", categories, "): "]
      categories = intercalate ", " . nub $ map applicationCategory apps

myApps :: [Application]
myApps = mconcat [myBrowsers, myReaders, mySoundUtils]

myBrowsers =
  [ Application "Browser" "Firefox" "firefox"
  , Application "Browser" "Chromium" "chromium"
  , Application "Browser" "Google-chrome" "google-chrome-stable"
  , Application "Browser" "Brave" "brave"
  , Application "Browser" "Qutebrowser" "qutebrowser"
  ]

myReaders =
  [ Application "Reader" "Zathura" "zathura"
  , Application "Reader" "Foliate" "foliate"
  , Application "Reader" "Evince" "evince"
  ]

mySoundUtils =
  [ Application "Sound" "Easyeffects" "easyeffects"
  , Application "Sound" "Pavucontrol" "pavucontrol"
  ]

selectAppByNameAndDo' :: XPConfig -> AppByName -> (String -> X ()) -> X ()
selectAppByNameAndDo' conf (AppByName apps) action = do
  cmds <- io getCommands

  let
    layoutCompFunc = aListCompFunc conf

    validApps = filter ((`elem` cmds) . applicationCommand) apps
    appNames = applicationName <$> validApps
    appCommands = applicationCommand <$> validApps
    appNames' = zip appNames appCommands

    lookupApp cmd =
      applicationCommand
        <$> find (\app -> cmd == applicationName app) validApps

  mkXPrompt
    (AppByName apps)
    conf
    (layoutCompFunc appNames')
    (action . fromJust . lookupApp)

selectAppByNameAndDo conf = selectAppByNameAndDo' conf (AppByName myApps)
selectAppByNameAndSpawn conf = selectAppByNameAndDo conf spawn

selectBrowserByNameAndDo conf =
  selectAppByNameAndDo' conf (AppByName myBrowsers)
selectBrowserByNameAndSpawn conf = selectBrowserByNameAndDo conf spawn

selectReaderByNameAndDo conf = selectAppByNameAndDo' conf (AppByName myReaders)
selectReaderByNameAndSpawn conf = selectReaderByNameAndDo conf spawn
