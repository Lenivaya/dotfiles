module XMonad.Custom.Projects
    ( projects
    ) where

import           XMonad.Actions.DynamicProjects
import           XMonad.Actions.SpawnOn
import qualified XMonad.Custom.Misc             as C

projects :: [Project]
projects =
    [ Project { projectName      = "Template"
              , projectDirectory = "~/"
              , projectStartHook = Nothing
              }

    , Project { projectName      = "Emacs"
              , projectDirectory = "~/"
              , projectStartHook = Just $ spawnOn "Emacs" (C.emacsGUI C.applications)
              }

    , Project { projectName      = "WWW"
              , projectDirectory = "~/"
              , projectStartHook = Just $ spawnOn "WWW" (C.browser C.applications)
              }

    , Project { projectName      = "Read"
              ,  projectDirectory = "~/"
              ,  projectStartHook = Just $ spawnOn "Read" (C.reader C.applications)
              }
    ]
