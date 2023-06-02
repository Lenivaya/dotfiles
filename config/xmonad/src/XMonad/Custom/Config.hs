{-# OPTIONS_GHC -Wno-missing-signatures #-}

module XMonad.Custom.Config (
  myConfig,
) where

import Flow
import XMonad
import XMonad.Actions.DynamicProjects (dynamicProjects)
import XMonad.Actions.MostRecentlyUsed
import XMonad.Actions.Navigation2D (withNavigation2DConfig)
import XMonad.Core
import XMonad.Custom.Bindings qualified as C
import XMonad.Custom.Event qualified as C
import XMonad.Custom.Layout qualified as C
import XMonad.Custom.Log qualified as C
import XMonad.Custom.Manage qualified as C
import XMonad.Custom.Misc qualified as C
import XMonad.Custom.Navigation qualified as C
import XMonad.Custom.Screens qualified as C
import XMonad.Custom.Startup qualified as C
import XMonad.Custom.Statusbar qualified as C
import XMonad.Custom.Theme qualified as C
import XMonad.Custom.Workspaces qualified as C
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.Rescreen
import XMonad.Hooks.StatusBar
import XMonad.Hooks.UrgencyHook
import XMonad.Layout.IndependentScreens

myConfig =
  def
    { borderWidth = C.border
    , workspaces = C.workspaces
    , layoutHook = C.layoutHook
    , terminal = C.term C.applications
    , normalBorderColor = C.colorN
    , focusedBorderColor = C.colorF
    , modMask = C.modMask
    , keys = C.keys
    , logHook = C.logHook
    , startupHook = C.startupHook
    , mouseBindings = C.mouseBindings
    , manageHook = C.manageHook
    , handleEventHook = C.handleEventHook
    , focusFollowsMouse = True
    , clickJustFocuses = False
    }
    |> dynamicProjects C.projects
    |> withNavigation2DConfig C.navigation
    |> withUrgencyHook (borderUrgencyHook C.red1)
    |> addRandrChangeHook C.myRandrChangeHook
    |> configureMRU
    |> ewmh
    |> ewmhFullscreen
    |> docks
    |> dynamicSBs C.barSpawner
    |> (return :: a -> IO a)
