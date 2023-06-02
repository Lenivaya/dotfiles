module XMonad.Custom.Workspaces (
  projects,
  workspaces,
) where

import Data.Foldable
import XMonad hiding (workspaces)
import XMonad.Actions.DynamicProjects
import XMonad.Actions.SpawnOn
import XMonad.Actions.WindowGo
import XMonad.Custom.ApplicationChooser
import XMonad.Custom.KeyboardUtils
import XMonad.Custom.Misc qualified as C
import XMonad.Custom.Prompt

(generic, code, web, wsread, sys, tmp, wsWRK, template, graphics, sound, vm, write) =
  ( "GEN"
  , "Code"
  , "WWW"
  , "Read"
  , "SYS"
  , "TMP"
  , "WRK"
  , "Template"
  , "GRAPH"
  , "SOUND"
  , "VM"
  , "WRITE"
  )

workspaces :: [String]
workspaces = [generic, sys, tmp, wsWRK, wsread, code, web]

projects :: [Project]
projects =
  [ Project
      { projectName = generic
      , projectDirectory = "~/"
      , projectStartHook = Nothing
      }
  , Project
      { projectName = template
      , projectDirectory = "~/"
      , projectStartHook = Just $ do
          spawnOn
            template
            (C.term C.applications ++ " -e tmux")
          spawnOn template (C.browser C.applications)
      }
  , Project
      { projectName = graphics
      , projectDirectory = "~/"
      , projectStartHook = Just $ do
          spawnOn graphics "gimp"
      }
  , Project
      { projectName = sound
      , projectDirectory = "~/"
      , projectStartHook = Just $ do
          spawnOn sound (C.soundEffects C.applications)
          spawnOn sound "pavucontrol"
      }
  , Project
      { projectName = vm
      , projectDirectory = "~/"
      , projectStartHook = Just $ do
          spawnOn vm (C.virtualMachinesManger C.applications)
      }
  , Project
      { projectName = write
      , projectDirectory = "~/"
      , projectStartHook = Just $ do
          spawnOn write "emacs_lets_write"
      }
  , Project
      { projectName = code
      , projectDirectory = "~/"
      , projectStartHook = Just raiseEditor
      }
  , Project
      { projectName = web
      , projectDirectory = "~/"
      , projectStartHook =
          Just $
            wrapKbdLayout $
              selectBrowserByNameAndDo
                promptTheme
                (spawnOn web)
      }
  , Project
      { projectName = wsread
      , projectDirectory = "~/"
      , projectStartHook =
          Just $
            wrapKbdLayout $
              selectReaderByNameAndDo
                promptTheme
                (spawnOn wsread)
      }
  , Project
      { projectName = sys
      , projectDirectory = "~/"
      , projectStartHook = Just $ do
          spawnOn sys (C.term C.applications)
          spawnOn sys (C.term C.applications)
      }
  , Project
      { projectName = tmp
      , projectDirectory = "~/"
      , projectStartHook = Nothing
      }
  , Project
      { projectName = wsWRK
      , projectDirectory = "~/"
      , projectStartHook = Nothing
      }
  ]
