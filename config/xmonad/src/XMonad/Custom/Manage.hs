module XMonad.Custom.Manage
    ( manageHook
    ) where

import           XMonad                  hiding ( manageHook )
import           XMonad.Custom.Scratchpads
import           XMonad.Hooks.InsertPosition
import           XMonad.Hooks.ManageDocks
import           XMonad.Hooks.ManageHelpers
import           XMonad.Layout.Fullscreen
import           XMonad.Util.NamedScratchpad

composeActions :: [MaybeManageHook]
composeActions =
    [ appName =? "emacs-popup" -?> tileBelowNoFocus
    , appName =? "eterm" -?> tileBelow
    , appName =? "spotify" -?> tileBelowNoFocus
    , className =? "Pinentry" -?> doCenterFloat
    , className =? "Emacs" <&&> title =? "emacs-anywhere" -?> doFullFloat
    , className =? "Steam" <&&> not <$> title =? "Steam" -?> doFloat
    , className =? "Xmessage" -?> doCenterFloat
    , className =? "Zenity" -?> doCenterFloat
    , className =? "explorer.exe" -?> doFullFloat
    , className =? "qemu-system-x86" -?> doCenterFloat
    , className =? "qemu-system-x86_64" -?> doCenterFloat
    , className =? "alacritty" -?> tileBelow
    , className =? "xterm" -?> tileBelow
    , className =? "st" -?> tileBelow
    , isDialog -?> doCenterFloat
    , isFullscreen -?> doFullFloat
    , pure True -?> tileNormal
    , stringProperty "WM_WINDOW_ROLE" =? "pop-up" -?> doCenterFloat
    , stringProperty "WM_WINDOW_ROLE"
        =?  "GtkFileChooserDialog"
        -?> doCenterFloat
    , transience
    ]
  where
    tileNormal       = insertPosition Above Newer
    tileBelow        = insertPosition Below Newer
    tileBelowNoFocus = insertPosition Below Older

manageHook :: ManageHook
manageHook = composeAll
    [ manageDocks
    , fullscreenManageHook
    , namedScratchpadManageHook scratchpads
    , composeOne composeActions
    ]
