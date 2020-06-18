{-# LANGUAGE LambdaCase #-}

module XMonad.Custom.Bindings
  ( keys
  , rawKeys
  , modMask
  , mouseBindings
  )
where

import qualified Data.Map                      as M
import           System.Exit
import           XMonad                  hiding ( keys
                                                , modMask
                                                , mouseBindings
                                                )
import           XMonad.Actions.CopyWindow
import           XMonad.Actions.CycleWS
import           XMonad.Actions.DynamicProjects
import           XMonad.Actions.DynamicWorkspaces
import qualified XMonad.Actions.FlexibleManipulate
                                               as F
import           XMonad.Actions.FloatSnap
import           XMonad.Actions.GridSelect
import           XMonad.Actions.MessageFeedback
import           XMonad.Actions.Navigation2D
import           XMonad.Actions.Promote
import           XMonad.Actions.UpdatePointer
import           XMonad.Actions.WithAll
import           XMonad.Actions.WindowGo
import           XMonad.Actions.WindowMenu
import           XMonad.Actions.Minimize
import           XMonad.Hooks.UrgencyHook
import           XMonad.Layout.BinarySpacePartition
import           XMonad.Layout.Hidden
import           XMonad.Layout.MultiToggle
import           XMonad.Layout.MultiToggle.Instances
import           XMonad.Layout.Reflect
import           XMonad.Layout.ResizableTile
import           XMonad.Layout.SubLayouts
import           XMonad.Layout.Maximize
import           XMonad.Prompt.ConfirmPrompt
import           XMonad.Prompt.Pass
import           XMonad.Prompt.Shell
import           XMonad.Prompt.Window
import           XMonad.Prompt.Workspace
import qualified XMonad.StackSet               as S
import           XMonad.Util.EZConfig
import           XMonad.Util.NamedScratchpad
import           XMonad.Util.WorkspaceCompare

-- Local
import           XMonad.Custom.Layout           ( selectLayoutByName
                                                , toggleLayout
                                                )
import           XMonad.Custom.Layout
import qualified XMonad.Custom.Misc            as C
import           XMonad.Custom.Prompt           ( promptTheme
                                                , hotPromptTheme
                                                )
import           XMonad.Custom.Scratchpads
import           XMonad.Actions.PerConditionKeys

modMask :: KeyMask
modMask = mod4Mask

directions :: [Direction2D]
directions = [D, U, L, R]

arrowKeys, directionKeys, wsKeys :: [String]
arrowKeys = ["<D>", "<U>", "<L>", "<R>"]
directionKeys = ["j", "k", "h", "l"]
wsKeys = map show [1 .. 9 :: Int]

zipKeys :: [a] -> [[a]] -> [t1] -> (t1 -> b) -> [([a], b)]
zipKeys m ks as f = zipWith (\k d -> (m ++ k, f d)) ks as
zipKeys' :: [a] -> [[a]] -> [t1] -> (t1 -> t2 -> b) -> t2 -> [([a], b)]
zipKeys' m ks as f b = zipWith (\k d -> (m ++ k, f d b)) ks as

tryMessageR_ :: (Message a, Message b) => a -> b -> X ()
tryMessageR_ x y = sequence_ [tryMessageWithNoRefreshToCurrent x y, refresh]

toggleCopyToAll :: X ()
toggleCopyToAll = wsContainingCopies >>= \case
  [] -> windows copyToAll
  _  -> killAllOtherCopies

getSortByIndexNonSP :: X ([WindowSpace] -> [WindowSpace])
getSortByIndexNonSP = (. namedScratchpadFilterOutWorkspace) <$> getSortByIndex

nextNonEmptyWS, prevNonEmptyWS :: X ()
nextNonEmptyWS = findWorkspace getSortByIndexNonSP Next HiddenNonEmptyWS 1
  >>= \t -> windows . S.view $ t
prevNonEmptyWS = findWorkspace getSortByIndexNonSP Prev HiddenNonEmptyWS 1
  >>= \t -> windows . S.view $ t

toggleFloat :: Window -> X ()
toggleFloat w = windows
  (\s -> if M.member w (S.floating s)
    then S.sink w s
    else S.float
      w
      (S.RationalRect (1 / 2 - 1 / 4) (1 / 2 - 1 / 4) (1 / 2) (1 / 2))
      s
  )

withUpdatePointer :: [(String, X ())] -> [(String, X ())]
withUpdatePointer = map addAction
 where
  addAction :: (String, X ()) -> (String, X ())
  addAction (key, action) = (key, action >> updatePointer (0.98, 0.01) (0, 0))

keys :: XConfig Layout -> M.Map (KeyMask, KeySym) (X ())
keys c = mkKeymap c (rawKeys c)

rawKeys :: XConfig Layout -> [(String, X ())]
rawKeys c = withUpdatePointer $ concatMap ($ c) keymaps
 where
  keymaps =
    [ keysBase
    , keysPass
    , keysSystem
    , keysMedia
    , keysWorkspaces
    , keysSpawnables
    , keysWindows
    , keysLayout
    , keysResize
    ]

keysBase :: XConfig Layout -> [(String, X ())]
keysBase _ =
  [ ("M-q q", confirmPrompt hotPromptTheme "Quit XMonad?" $ io exitSuccess)
  , ("M-q r", spawn "xmonad --restart")
  , ("M-x"  , shellPrompt promptTheme)
  , ("M-S-x", spawn (C.appmenu C.applications))
  ]

keysPass :: XConfig Layout -> [(String, X ())]
keysPass _ =
  [ ("M-p p", passPrompt promptTheme)
  , ("M-p g", passGenerateAndCopyPrompt promptTheme)
  , ("M-p d", passRemovePrompt promptTheme)
  , ("M-p t", passTypePrompt promptTheme)
  ]

keysSystem :: XConfig Layout -> [(String, X ())]
keysSystem _ =
  [ ("<Print>"  , spawn "~/.dotfiles/config/xmonad/scripts/xshot.sh")
  , ("S-<Print>", spawn "~/.dotfiles/config/xmonad/scripts/xshot-select.sh")
  , ( "C-S-<Print>"
    , spawn "~/.dotfiles/config/xmonad/scripts/xshot-select-clipboard.sh"
    )
  , ("M-t c", spawn "~/.dotfiles/config/xmonad/scripts/toggle-compositor.sh")
  ]

keysMedia :: XConfig Layout -> [(String, X ())]
keysMedia _ =
  [ ("<XF86AudioMicMute>"    , spawn "pulsemixer --toggle-mute --id 1")
  , ("<XF86AudioMute>"       , spawn "pulsemixer --toggle-mute --id 0")
  , ("<XF86AudioLowerVolume>", spawn "pulsemixer --change-volume -10 --id 0")
  , ("<XF86AudioRaiseVolume>", spawn "pulsemixer --change-volume +10 --id 0")
  , ("<XF86AudioStop>"       , spawn "mpc stop")
  , ("<XF86AudioPrev>"       , spawn "mpc prev")
  , ("<XF86AudioNext>"       , spawn "mpc next")
  ]

keysWorkspaces :: XConfig Layout -> [(String, X ())]
keysWorkspaces _ =
  [ ("M-w S-o", switchProjectPrompt promptTheme)
    , ("M-w S-p", shiftToProjectPrompt promptTheme)
    , ("M-w S-n", renameProjectPrompt promptTheme)
    , ("M-,"    , nextNonEmptyWS)
    , ("M-."    , prevNonEmptyWS)
    , ("M-i"    , toggleWS' ["NSP"])
    , ("M-n", workspacePrompt promptTheme $ windows . S.shift)
    ]
    ++ zipKeys "M-"     wsKeys [0 ..] (withNthWorkspace S.greedyView)
    ++ zipKeys "M-S-"   wsKeys [0 ..] (withNthWorkspace S.shift)
    ++ zipKeys "M-C-S-" wsKeys [0 ..] (withNthWorkspace copy)

keysSpawnables :: XConfig Layout -> [(String, X ())]
keysSpawnables _ =
  [ ("M-<Return>"  , spawn (C.term C.applications))
  , ("M-S-<Return>", spawn ((C.term C.applications) ++ " -e tmux"))
  , ("M-o b"       , spawn (C.browser C.applications))
  , ("M-o e"       , raiseEditor)
  , ("M-o c", namedScratchpadAction scratchpads "console")
  , ("M-o m"       , namedScratchpadAction scratchpads "music")
  , ("M-o t"       , namedScratchpadAction scratchpads "top")
  , ("M-o v", namedScratchpadAction scratchpads "volume")
  ]

keysWindows :: XConfig Layout -> [(String, X ())]
keysWindows _ =
  [ ("M-w k"  , kill)
    , ("M-w S-k", confirmPrompt hotPromptTheme "Kill all" killAll)
    , ("M-w d"  , windowMenu)
    , ("M-w g", windowPrompt promptTheme Goto allWindows)
    , ("M-w b", windowPrompt promptTheme Bring allWindows)
    , ("M-w c"  , toggleCopyToAll)
    , ( "M-w S-c"
      , kill1
      ) -- To remove focused copied window from current workspace
    , ("M-w h"  , withFocused minimizeWindow)
    , ("M-w S-h", withLastMinimized maximizeWindowAndFocus)
    , ("M-w p"  , promote)
    , ("M-w t"  , withFocused $ sendMessage . MergeAll)
    , ("M-w S-t", withFocused $ sendMessage . UnMerge)
    , ("M-w u"  , focusUrgent)
    , ("M-w m"  , windows S.focusMaster)
    , ("M-w S-m", windows S.swapMaster)
    , ( "M-'"
      , bindOn LD [("Tabs", windows S.focusDown), ("", onGroup S.focusDown')]
      )
    , ("M-;", bindOn LD [("Tabs", windows S.focusUp), ("", onGroup S.focusUp')])
    , ("M-S-'", windows S.swapDown)
    , ("M-S-;", windows S.swapUp)
    ]
    ++ zipKeys' "M-"   directionKeys directions windowGo   True
    ++ zipKeys' "M-S-" directionKeys directions windowSwap True
    ++ zipKeys "M-C-" directionKeys directions (sendMessage . pullGroup)
    ++ zipKeys' "M-"   arrowKeys directions screenGo       True
    ++ zipKeys' "M-S-" arrowKeys directions windowToScreen True
    ++ zipKeys' "M-C-" arrowKeys directions screenSwap     True

keysLayout :: XConfig Layout -> [(String, X ())]
keysLayout c =
  [ ("M-<Tab>"      , sendMessage NextLayout)
  , ("M-C-<Tab>"    , toSubl NextLayout)
  , ("M-S-<Tab>"    , setLayout $ XMonad.layoutHook c)
  , ("M-<Backspace>", selectLayoutByName promptTheme)
  , ("M-y"          , withFocused toggleFloat)
  , ("M-S-y"        , sinkAll)
  , ("M-S-,"        , sendMessage $ IncMasterN (-1))
  , ("M-S-."        , sendMessage $ IncMasterN 1)
  , ("M-r"          , tryMessageR_ Rotate (Toggle REFLECTX))
  , ("M-S-r"        , sendMessage $ Toggle REFLECTX)
  , ( "M-f"
    , sequence_ [withFocused $ windows . S.sink, sendMessage $ Toggle NBFULL]
    )
  , ("M-S-f", withFocused (sendMessage . maximizeRestore))
  , ("M-C-g", sendMessage $ Toggle GAPS) -- FIXME Breaks merged tabbed layout
  ]

keysResize :: XConfig Layout -> [(String, X ())]
keysResize _ =
  [ ("M-["    , tryMessageR_ (ExpandTowards L) Shrink)
  , ("M-]"    , tryMessageR_ (ExpandTowards R) Expand)
  , ("M-S-["  , tryMessageR_ (ExpandTowards U) MirrorShrink)
  , ("M-S-]"  , tryMessageR_ (ExpandTowards D) MirrorExpand)
  , ("M-C-["  , tryMessageR_ (ShrinkFrom R) Shrink)
  , ("M-C-]"  , tryMessageR_ (ShrinkFrom L) Expand)
  , ("M-S-C-[", tryMessageR_ (ShrinkFrom D) MirrorShrink)
  , ("M-S-C-]", tryMessageR_ (ShrinkFrom U) MirrorExpand)
  ]

mouseBindings :: XConfig Layout -> M.Map (KeyMask, Button) (Window -> X ())
mouseBindings XConfig{} = M.fromList
 -- mod-button1, flexible linear scale
  [ ( (mod4Mask, button1)
    , \w -> focus w >> F.mouseWindow F.discrete w
    )
    -- mod-button2, Raise the window to the top of the stack
  , ( (mod4Mask, button2)
    , \w -> focus w >> windows S.shiftMaster
    )
    -- mod-button3, Set the window to floating mode and resize by dragging
  , ( (mod4Mask, button3)
    , \w -> focus w >> mouseResizeWindow w >> windows S.shiftMaster
    )
    -- you may also bind events to the mouse scroll wheel (button4 and button5)
  ]
