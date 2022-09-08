{-# OPTIONS_GHC -Wno-missing-signatures #-}

module XMonad.Custom.Config
  ( myConfig
  ) where

import           Flow
import           XMonad
import           XMonad.Actions.DynamicProjects ( dynamicProjects )
import           XMonad.Actions.Navigation2D    ( withNavigation2DConfig )
import qualified XMonad.Custom.Bindings        as C
import qualified XMonad.Custom.Event           as C
import qualified XMonad.Custom.Layout          as C
import qualified XMonad.Custom.Log             as C
import qualified XMonad.Custom.Manage          as C
import qualified XMonad.Custom.Misc            as C
import qualified XMonad.Custom.Navigation      as C
import qualified XMonad.Custom.Startup         as C
import qualified XMonad.Custom.Theme           as C
import qualified XMonad.Custom.Workspaces      as C
import           XMonad.Hooks.EwmhDesktops
import           XMonad.Hooks.ManageDocks
import           XMonad.Hooks.UrgencyHook
import           XMonad.Layout.IndependentScreens

myConfig = do
  screens <- countScreens
  def { borderWidth        = C.border
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
    |> dynamicProjects C.projects
    |> withUrgencyHook (borderUrgencyHook C.red1)
    |> withNavigation2DConfig C.navigation
    |> ewmh
    |> ewmhFullscreen
    |> docks
    |> (return :: a -> IO a)
