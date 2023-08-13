module XMonad.Custom.Manage.ManageHook (
  manageHook,
  activateHook,
) where

import XMonad hiding (manageHook)
import XMonad.Actions.SpawnOn
import XMonad.Custom.Manage.ManageHelpers
import XMonad.Custom.Scratchpads
import XMonad.Hooks.InsertPosition
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.UrgencyHook
import XMonad.Layout.Fullscreen
import XMonad.Layout.NoBorders
import XMonad.StackSet as W
import XMonad.Util.NamedScratchpad

activateHook :: ManageHook
activateHook =
  mconcat
    [ isDialog --> doAskUrgent
    , className =? "mpv" --> doAskUrgent
    , className =? "help" --> doAskUrgent
    , className =? "Zathura" --> doAskUrgent
    , className =? "Emacs" --> doAskUrgent
    ]

composeActions :: [MaybeManageHook]
composeActions =
  [ appName =? "emacs-popup" -?> tileBelowNoFocus
  , appName =? "eterm" -?> tileBelow
  , appName =? "spotify" -?> doFullCenterFloat
  , appName =? "emacs" <&&> title =? "emacs-anywhere" -?> centerFloat 0.5 0.5
  , appName =? "ulauncher" -?> noBorder
  , className =? "mpv" -?> tileNormal
  , className =? "Pinentry" -?> doCenterFloat
  , className =? "Steam" <&&> not <$> title =? "Steam" -?> doCenterFloat
  , className =? "Xmessage" -?> doCenterFloat
  , className =? "Zenity" -?> doCenterFloat
  , className =? "explorer.exe" -?> doFullFloat
  , className =? "qemu-system-x86" -?> doCenterFloat
  , className =? "qemu-system-x86_64" -?> doCenterFloat
  , className =? "Safeeyes" -?> doFullFloat
  , className =? "Avizo-service" -?> doIgnore
  , isRole =? "GtkFileChooserDialog" -?> doCenterFloat
  , isRole =? "pop-up" -?> doCenterFloat
  , isRole =? "About" -?> doCenterFloat
  , isDialog -?> doCenterFloat
  , transience
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
    [ manageDocks
    , fullscreenManageHook
    , manageSpawn
    , namedScratchpadManageHook scratchpads
    , composeOne composeActions
    ]
