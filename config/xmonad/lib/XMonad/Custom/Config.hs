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
import XMonad.Custom.Hooks.Event qualified as C
import XMonad.Custom.Hooks.Layout qualified as C
import XMonad.Custom.Hooks.Log qualified as C
import XMonad.Custom.Hooks.Screens qualified as C
import XMonad.Custom.Hooks.Startup qualified as C
import XMonad.Custom.Hooks.Statusbar qualified as C
import XMonad.Custom.Manage.ManageHook qualified as C
import XMonad.Custom.Misc qualified as C
import XMonad.Custom.Navigation qualified as C
import XMonad.Custom.Scratchpads qualified as C
import XMonad.Custom.Theme qualified as C
import XMonad.Custom.Workspaces qualified as C
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.Rescreen
import XMonad.Hooks.StatusBar
import XMonad.Hooks.UrgencyHook
import XMonad.Layout.IndependentScreens
import XMonad.Util.Hacks qualified as Hacks
import XMonad.Util.Loggers.NamedScratchpad

myConfig =
        def
                { borderWidth = C.border
                , workspaces = C.workspaces
                , layoutHook = C.layoutHook
                , terminal = C.term C.applications
                , normalBorderColor = C.colorN
                , focusedBorderColor = C.colorF
                , modMask = C.modMask
                , keys = C.myKeys
                , logHook = C.logHook
                , startupHook = mconcat [C.startupHook, nspTrackStartup C.scratchpads]
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
                |> Hacks.javaHack
                |> (return :: a -> IO a)
