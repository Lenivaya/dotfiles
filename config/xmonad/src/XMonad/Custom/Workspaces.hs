module XMonad.Custom.Workspaces
  ( projects
  , workspaces
  , selectBrowserByName
  ) where

import           Data.Foldable

import           XMonad                  hiding ( workspaces )
import           XMonad.Actions.DynamicProjects
import           XMonad.Actions.SpawnOn
import           XMonad.Actions.WindowGo
import qualified XMonad.Custom.Misc            as C
-- browser prompt
import           XMonad.Custom.Prompt           ( aListCompFunc
                                                , promptTheme
                                                )
import           XMonad.Prompt
import           XMonad.Prompt.Shell

(generic, code, template, web, wsread, sys, tmp, wsWRK) =
  ("GEN", "Code", "Template", "WWW", "Read", "SYS", "TMP", "WRK")

workspaces :: [String]
workspaces = [generic, sys, tmp, wsWRK, wsread, code, web]

projects :: [Project]
projects =
  [ Project { projectName      = generic
            , projectDirectory = "~/"
            , projectStartHook = Nothing
            }
  , Project
    { projectName      = template
    , projectDirectory = "~/"
    , projectStartHook = Just $ do
                           spawnOn wsWRK (C.term C.applications ++ " -e tmux")
                           spawnOn wsWRK (C.browser C.applications)
    }
  , Project { projectName      = code
            , projectDirectory = "~/"
            , projectStartHook = Just raiseEditor
            }
  , Project { projectName      = web
            , projectDirectory = "~/"
            , projectStartHook = Just $ selectBrowserByName promptTheme
            }
  , Project { projectName      = wsread
            , projectDirectory = "~/"
            , projectStartHook = Just $ selectReaderByName promptTheme
            }
  , Project
    { projectName      = sys
    , projectDirectory = "~/"
    , projectStartHook = Just $ do
                           spawnOn sys (C.term C.applications)
                           spawnOn sys (C.term C.applications)
    }
  , Project { projectName      = tmp
            , projectDirectory = "~/"
            , projectStartHook = Nothing
            }
  , Project { projectName      = wsWRK
            , projectDirectory = "~/"
            , projectStartHook = Nothing
            }
  ]

--------------------------------------------------------------------------------

-- | Use @Prompt@ to choose a browser which then will be runned on WWW.
data BrowserByName = BrowserByName

instance XPrompt BrowserByName where
  showXPrompt BrowserByName = "Browser: "

selectBrowserByName :: XPConfig -> X ()
selectBrowserByName conf = do
  cmds <- io getCommands
  mkXPrompt
    BrowserByName
    conf
    (aListCompFunc conf $ filter (\(x, y) -> y `elem` cmds) browserNames)
    go
 where
  go :: String -> X ()
  go selected = forM_ (lookup selected browserNames) (spawnOn web)

  browserNames :: [(String, String)]
  browserNames =
    [ ("Firefox"    , "firefox")
    , ("Chromium"   , "chromium")
    , ("Brave"      , "brave")
    , ("Qutebrowser", "qutebrowser")
    ]



-- | Use @Prompt@ to choose a reader which then will be runned on Read workspace.
data ReaderByName = ReaderByName

instance XPrompt ReaderByName where
  showXPrompt ReaderByName = "Reader: "

selectReaderByName :: XPConfig -> X ()
selectReaderByName conf = do
  cmds <- io getCommands
  mkXPrompt
    ReaderByName
    conf
    (aListCompFunc conf $ filter (\(x, y) -> y `elem` cmds) readerNames)
    go
 where
  go :: String -> X ()
  go selected = forM_ (lookup selected readerNames) (spawnOn wsread)
  -- case lookup selected readerNames of
  --   Nothing      -> return ()
  --   Just reader -> spawnOn wsread reader

  readerNames :: [(String, String)]
  readerNames =
    [("Zathura", "zathura"), ("Foliate", "foliate"), ("Evince", "evince")]
