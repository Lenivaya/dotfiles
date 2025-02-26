{-# LANGUAGE ImportQualifiedPost #-}

module XMonad.Custom.MouseBindings (
  mouseBindings,
) where

import Data.Map qualified as M
import XMonad hiding (
  keys,
  modMask,
  mouseBindings,
 )
import XMonad.Actions.FlexibleManipulate qualified as Flex
import XMonad.Actions.Minimize
import XMonad.Actions.TiledWindowDragging
import XMonad.StackSet qualified as S
import XMonad.Util.EZConfig

type Mousebindings = M.Map (ButtonMask, Button) (Window -> X ())

mouseBindings :: XConfig Layout -> Mousebindings
mouseBindings config =
  M.fromList
    [ -- mod-button1, flexible linear scale
      ((mod4Mask, button1), \w -> focus w >> Flex.mouseWindow Flex.discrete w),
      -- mod-button2, Raise the window to the top of the stack
      ((mod4Mask, button2), \w -> focus w >> windows S.shiftMaster),
      -- mod-button3, Set the window to floating mode and resize by dragging
      ( (mod4Mask, button3),
        \w -> focus w >> mouseResizeWindow w >> windows S.shiftMaster
      ),
      -- you may also bind events to the mouse scroll wheel (button4 and button5)
      ((mod4Mask, button4), \w -> withFocused minimizeWindow),
      ((mod4Mask, button5), \w -> withLastMinimized maximizeWindowAndFocus),
      -- Dragging of tiled windows
      ((mod4Mask .|. shiftMask, button1), dragWindow)
      --- Some touchpad things
      -- -- Go to next workspace on left scroll
      -- ((mod4Mask, 7), const nextNonEmptyWS),
      -- -- Go to previous workspace on right scroll
      -- ((mod4Mask, 6), const prevNonEmptyWS)
    ]
