module XMonad.Custom.Manage.ManageHook (
  manageHook,
) where

import XMonad hiding (manageHook)
import XMonad.Actions.SpawnOn
import XMonad.Custom.Manage.ManageHelpers
import XMonad.Custom.Scratchpads
import XMonad.Custom.Workspaces (git)
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.InsertPosition
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.UrgencyHook
import XMonad.Layout.Fullscreen
import XMonad.Layout.NoBorders
import XMonad.StackSet as W
import XMonad.Util.NamedScratchpad

composeActions :: [MaybeManageHook]
composeActions =
  [ appName =? "emacs-popup" -?> tileBelowNoFocus,
    appName =? "eterm" -?> tileBelow,
    appName =? "spotify" -?> doFullCenterFloat,
    appName =? "emacs" <&&> title =? "emacs-anywhere" -?> centerFloat 0.5 0.5,
    appName =? "ulauncher" -?> noBorder,
    appName =? "Junction" -?> doCenterFloat,
    className =? "mpv" -?> tileNormal,
    className =? "Pinentry" -?> doCenterFloat,
    className =? "pinentry-gtk-2" -?> doCenterFloat,
    className =? "Steam" <&&> not <$> title =? "Steam" -?> doCenterFloat,
    className =? "Xmessage" -?> doCenterFloat,
    className =? "Zenity" -?> doCenterFloat,
    className =? "explorer.exe" -?> doFullFloat,
    className =? "qemu-system-x86" -?> doCenterFloat,
    className =? "qemu-system-x86_64" -?> doCenterFloat,
    className =? "re.sonny.Junction" -?> doCenterFloat,
    className =? "gcr-prompter" <||> className =? "Gcr-prompter" -?> doCenterFloat,
    className =? "Safeeyes" -?> doFullFloat,
    className =? "Avizo-service" -?> doIgnore,
    className =? "gitbutler-tauri" -?> doShift git,
    className
      =? "jetbrains-toolbox"
      <||> appName
      =? "JetBrains Toolbox"
      <||> title
      =? "JetBrains Toolbox"
      -?> doCenterFloat,
    isRole =? "GtkFileChooserDialog" -?> doCenterFloat,
    isRole =? "pop-up" -?> doCenterFloat,
    isRole =? "About" -?> doCenterFloat,
    isDialog -?> doCenterFloat,
    stringProperty "WM_WINDOW_ROLE" =? "browser" -?> ewmhDesktopsManageHook,
    transience
  ]
  where
    tileNormal = insertPosition Above Newer
    tileBelow = insertPosition Below Newer
    tileBelowNoFocus = insertPosition Below Older
    doFullCenterFloat = centerFloat 0.8 0.8
    noBorder = hasBorder False

manageHook :: ManageHook
manageHook =
  composeAll
    [ manageDocks,
      fullscreenManageHook,
      manageSpawn,
      composeOne composeActions,
      namedScratchpadManageHook scratchpads
    ]
