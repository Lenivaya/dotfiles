module XMonad.Custom.Projects
  ( projects
  , workspaces
  )
where

import           XMonad                  hiding ( workspaces )
import           XMonad.Actions.DynamicProjects
import           XMonad.Actions.SpawnOn
import qualified XMonad.Custom.Misc            as C

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
  , Project { projectName      = template
            , projectDirectory = "~/"
            , projectStartHook = Nothing
            }
  , Project { projectName      = code
            , projectDirectory = "~/"
            , projectStartHook = Just $ spawnOn code (C.emacs C.applications)
            }
  , Project { projectName      = web
            , projectDirectory = "~/"
            , projectStartHook = Just $ spawnOn web (C.browser C.applications)
            }
  , Project { projectName      = wsread
            , projectDirectory = "~/"
            , projectStartHook = Just $ spawnOn wsread (C.reader C.applications)
            }
  , Project
    { projectName      = sys
    , projectDirectory = "~/"
    , projectStartHook = Just $ do
                           spawnOn sys (C.term C.applications)
                           spawnOn sys (C.term C.applications)
    }
  , Project
    { projectName      = tmp
    , projectDirectory = "~/"
    , projectStartHook = Just $ do
                           return ()
    }
  , Project
    { projectName      = wsWRK
    , projectDirectory = "~/"
    , projectStartHook = Just $ do
                           spawnOn wsWRK (C.term C.applications)
                           spawnOn wsWRK (C.browser C.applications)
    }
  ]
