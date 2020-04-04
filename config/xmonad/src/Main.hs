module Main where

import           XMonad
import           XMonad.Actions.DynamicProjects
import           XMonad.Actions.Navigation2D
import qualified XMonad.Custom.Bindings         as C
import qualified XMonad.Custom.Event            as C
import qualified XMonad.Custom.Layout           as C
import qualified XMonad.Custom.Log              as C
import qualified XMonad.Custom.Manage           as C
import qualified XMonad.Custom.Misc             as C
import qualified XMonad.Custom.Navigation       as C
import qualified XMonad.Custom.Projects         as C
import qualified XMonad.Custom.Startup          as C
import qualified XMonad.Custom.Theme            as C
import           XMonad.Hooks.EwmhDesktops
import           XMonad.Hooks.ManageDocks
import           XMonad.Layout.Fullscreen

main :: IO ()
main = xmonad
       $ ewmh
       $ fullscreenSupport
       $ docks
       $ withNavigation2DConfig C.navigation
       $ dynamicProjects C.projects
       $ def { borderWidth        = C.border
             , workspaces         = C.workspaces
             , layoutHook         = C.layoutHook
             , terminal           = C.term C.applications
             , normalBorderColor  = C.colorN
             , focusedBorderColor = C.colorF
             , modMask            = C.modMask
             , keys               = C.keys
             , logHook            = C.logHook
             , startupHook        = C.startupHook
             , mouseBindings      = C.mouseBindings
             , manageHook         = C.manageHook
             , handleEventHook    = C.handleEventHook
             , focusFollowsMouse  = True
             , clickJustFocuses   = False
             }
