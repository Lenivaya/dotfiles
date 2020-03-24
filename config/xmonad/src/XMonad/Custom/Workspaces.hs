module XMonad.Custom.Workspaces
    ( workspaces
    ) where

import           XMonad.Actions.DynamicProjects
import           XMonad.Core                    hiding (workspaces)

workspaces :: [WorkspaceId]
workspaces = map show [1..9 :: Int]

projects :: [Project]
projects =
    [ Project { projectName      = "scratch"
              , projectDirectory = "~/"
              , projectStartHook = Nothing
              }

    , Project { projectName      = "www"
              , projectDirectory = "~/"
              , projectStartHook = Nothing
              }

    , Project { projectName      = "mail"
              , projectDirectory = "~/"
              , projectStartHook = Nothing
              }
    ]
