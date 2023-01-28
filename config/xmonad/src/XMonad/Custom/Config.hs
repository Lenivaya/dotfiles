{-# OPTIONS_GHC -Wno-missing-signatures #-}

module XMonad.Custom.Config
  ( myConfig
  ) where

import           Flow
import           XMonad
import           XMonad.Actions.DynamicProjects ( dynamicProjects )
import           XMonad.Actions.MostRecentlyUsed
import           XMonad.Actions.Navigation2D    ( withNavigation2DConfig )
import           XMonad.Core
import qualified XMonad.Custom.Bindings        as C
import qualified XMonad.Custom.Event           as C
import qualified XMonad.Custom.Layout          as C
import qualified XMonad.Custom.Log             as C
import qualified XMonad.Custom.Manage          as C
import qualified XMonad.Custom.Misc            as C
import qualified XMonad.Custom.Navigation      as C
import qualified XMonad.Custom.Screens         as C
import qualified XMonad.Custom.Startup         as C
import qualified XMonad.Custom.Statusbar       as C
import qualified XMonad.Custom.Theme           as C
import qualified XMonad.Custom.Workspaces      as C
import           XMonad.Hooks.EwmhDesktops
import           XMonad.Hooks.ManageDocks
import           XMonad.Hooks.Rescreen
import           XMonad.Hooks.StatusBar
import           XMonad.Hooks.UrgencyHook
import           XMonad.Layout.IndependentScreens


myConfig =
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
    |> withNavigation2DConfig C.navigation
    |> dynamicProjects C.projects
    |> withUrgencyHook (borderUrgencyHook C.red1)
    |> addRandrChangeHook C.myRandrChangeHook
    |> configureMRU
    |> ewmh
    |> ewmhFullscreen
    |> docks
    |> dynamicSBs C.barSpawner
    |> (return :: a -> IO a)
