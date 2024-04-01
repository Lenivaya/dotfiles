{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-deprecations #-}

module XMonad.Custom.Bindings (
  myKeys,
  modMask,
  mouseBindings,
) where

import Data.Foldable
import Data.Map qualified as M
import System.Exit
import XMonad hiding (
  keys,
  modMask,
  mouseBindings,
 )
import XMonad.Actions.CopyWindow
import XMonad.Actions.CycleWS
import XMonad.Actions.DwmPromote
import XMonad.Actions.DynamicProjects
import XMonad.Actions.DynamicWorkspaces
import XMonad.Actions.EasyMotion (selectWindow)
import XMonad.Actions.FlexibleManipulate qualified as Flex
import XMonad.Actions.FloatSnap
import XMonad.Actions.GridSelect
import XMonad.Actions.GroupNavigation
import XMonad.Actions.MessageFeedback
import XMonad.Actions.Minimize
import XMonad.Actions.MostRecentlyUsed
import XMonad.Actions.Navigation2D
import XMonad.Actions.PerLayoutKeys
import XMonad.Actions.ShowText
import XMonad.Actions.SwapPromote
import XMonad.Actions.TiledWindowDragging
import XMonad.Actions.UpdatePointer
import XMonad.Actions.WindowGo
import XMonad.Actions.WindowMenu
import XMonad.Actions.WithAll
import XMonad.Custom.Actions.ApplicationChooser
import XMonad.Custom.Actions.Calculator
import XMonad.Custom.Actions.Keyboard
import XMonad.Custom.Actions.LayoutChooser
import XMonad.Custom.Actions.Minimize
import XMonad.Custom.Actions.Screen.Screencast
import XMonad.Custom.Actions.Screen.Screenshot
import XMonad.Custom.Hooks.Layout
import XMonad.Custom.Misc qualified as C
import XMonad.Custom.Prompt
import XMonad.Custom.Scratchpads
import XMonad.Custom.Search
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.UrgencyHook
import XMonad.Layout.BinarySpacePartition
import XMonad.Layout.Hidden
import XMonad.Layout.Magnifier qualified as Mag
import XMonad.Layout.Maximize
import XMonad.Layout.MultiToggle
import XMonad.Layout.MultiToggle.Instances
import XMonad.Layout.Reflect
import XMonad.Layout.ResizableTile
import XMonad.Layout.SubLayouts
import XMonad.Operations (restart)
import XMonad.Prompt (XPConfig (..))
import XMonad.Prompt.ConfirmPrompt
import XMonad.Prompt.Man
import XMonad.Prompt.Pass
import XMonad.Prompt.Shell
import XMonad.Prompt.Window
import XMonad.Prompt.Workspace
import XMonad.StackSet qualified as S
import XMonad.Util.EZConfig
import XMonad.Util.NamedActions
import XMonad.Util.NamedScratchpad hiding (
  namedScratchpadFilterOutWorkspace,
 )
import XMonad.Util.WorkspaceCompare

type Keybinding = (String, X ())
type Keybindings = [Keybinding]

type Mousebindings = M.Map (ButtonMask, Button) (Window -> X ())

modMask :: KeyMask
modMask = mod4Mask

zipKeys :: [a] -> [[a]] -> [t1] -> (t1 -> b) -> [([a], b)]
zipKeys m ks as f = zipWith (\k d -> (m ++ k, f d)) ks as

zipKeys' :: [a] -> [[a]] -> [t1] -> (t1 -> t2 -> b) -> t2 -> [([a], b)]
zipKeys' m ks as f b = zipWith (\k d -> (m ++ k, f d b)) ks as

tryMessageR_ :: (Message a, Message b) => a -> b -> X ()
tryMessageR_ x y = sequence_ [tryMessageWithNoRefreshToCurrent x y, refresh]

toggleCopyToAll :: X ()
toggleCopyToAll =
  wsContainingCopies >>= \case
    [] -> windows copyToAll
    _ -> killAllOtherCopies

getSortByIndexNonSP :: X ([WindowSpace] -> [WindowSpace])
getSortByIndexNonSP = (. namedScratchpadFilterOutWorkspace) <$> getSortByIndex

nextNonEmptyWS, prevNonEmptyWS :: X ()
nextNonEmptyWS =
  findWorkspace getSortByIndexNonSP Next HiddenNonEmptyWS 1 >>= windows . S.view
prevNonEmptyWS =
  findWorkspace getSortByIndexNonSP Prev HiddenNonEmptyWS 1 >>= windows . S.view

toggleFloat :: Window -> X ()
toggleFloat win = windows $ \windowSet ->
  if M.member win (S.floating windowSet)
    then sinkWindow win windowSet
    else floatWindow win defaultRect windowSet
  where
    sinkWindow = S.sink
    floatWindow = S.float
    defaultRect = S.RationalRect (1 / 2 - 1 / 4) (1 / 2 - 1 / 4) (1 / 2) (1 / 2)

integrateOthers :: S.Stack a -> [a]
integrateOthers (S.Stack _ u d) = u <> d

withOthers :: (Window -> X ()) -> X ()
withOthers f = withWindowSet $ mapM_ f . others
  where
    others =
      maybe [] integrateOthers
        . S.stack
        . S.workspace
        . S.current

myKeys config = mkKeymap config keys
  where
    keys =
      mconcat
        [ keysBase,
          keysPass,
          keysGo,
          keysDo,
          keysSystem,
          keysWorkspaces,
          keysSpawnables,
          keysWindows,
          keysLayout config,
          keysResize,
          keysSelect
        ]

flash' = flashText def 0.5

keysBase :: Keybindings
keysBase =
  [ ("M-q q", confirmPrompt hotPromptTheme "Quit XMonad?" $ io exitSuccess),
    ("M-q r", restart "xmonad" True),
    ("M-x", wrapKbdLayout $ shellPrompt $ promptNoCompletion promptTheme),
    ("M-S-x", spawn $ C.appmenu C.applications),
    -- , ("M-c", spawn $ C.clipboardSelector C.applications)
    ("M1-<Tab>", mostRecentlyUsed [xK_Alt_L, xK_Alt_R] xK_Tab)
  ]

keysSelect :: Keybindings
keysSelect =
  [ ("M-s k", selectKbdLayout $ promptNoCompletion promptTheme),
    ("M-s l", wrapKbdLayout $ selectLayoutByName promptTheme)
  ]

keysPass :: Keybindings
keysPass =
  [ ("M-p p", wrapKbdLayout $ passPrompt promptTheme),
    ( "M-p g",
      wrapKbdLayout . passGenerateAndCopyPrompt $ promptNoCompletion promptThemeVim
    ),
    ("M-p d", wrapKbdLayout $ passRemovePrompt $ promptNoCompletion promptTheme),
    ("M-p t", wrapKbdLayout $ passTypePrompt promptTheme),
    ("M-p e", wrapKbdLayout $ passEditPrompt promptTheme)
  ]

keysGo :: Keybindings
keysGo =
  [ -- ("M-g s", mySearch)

    ( "M-g s",
      wrapKbdLayout . selectAndSearchPrompt $ promptNoCompletion promptTheme
    ),
    ("M-g m", wrapKbdLayout . manPrompt $ promptNoCompletion promptTheme),
    ("M-g c", wrapKbdLayout $ calcPrompt (promptNoCompletion promptTheme) ""),
    ("M-g t", wrapKbdLayout $ spawn "dmenu-tmux")
  ]

keysDo :: Keybindings
keysDo =
  [ ("M-d s s", wrapKbdLayout . screenshotPrompt $ promptNoCompletion promptTheme),
    ("M-d s r", wrapKbdLayout . screencastPrompt $ promptNoCompletion promptTheme),
    ("M-d s z", spawn $ C.screenZoomer C.applications),
    ("M-d w c", workspacePrompt promptTheme $ windows . copy)
  ]

keysSystem :: Keybindings
keysSystem =
  [ ("<Print>", screenshot Fullscreen),
    ("S-<Print>", screenshot Select),
    ("C-S-<Print>", screenshot SelectCopyToClipboard),
    ("M-t c", spawn "$XMONAD_CONFIG_DIR/scripts/toggle-compositor.sh"),
    ("M-t l", flash' "💤" >> spawn "zzz"),
    ("M-t S-l", flash' "☕" >> spawn "$XMONAD_CONFIG_DIR/scripts/caffeine")
  ]

keysWorkspaces :: Keybindings
keysWorkspaces =
  [ ("M-w S-o", withDefaultKbdLayout $ switchProjectPrompt promptTheme),
    ( "M-w C-S-o",
      withDefaultKbdLayout . switchProjectPrompt $ promptNoCompletion promptTheme
    ),
    ("M-w S-s", wrapKbdLayout $ shiftToProjectPrompt promptTheme),
    ("M-w S-n", wrapKbdLayout $ renameProjectPrompt hotPromptTheme),
    ("M-w <Backspace>", removeWorkspace),
    ( "M-w S-<Backspace>",
      confirmPrompt hotPromptTheme "Kill workspace?" $ killAll >> removeWorkspace
    ),
    ("M-,", flash' "->" >> nextNonEmptyWS),
    ("M-.", flash' "<-" >> prevNonEmptyWS),
    ("M-i", flash' "↻" >> toggleWS' ["NSP"]),
    ("M-n", workspacePrompt promptTheme $ windows . S.shift),
    ("M-w w", gridselectWorkspace gridSelectTheme S.greedyView)
  ]
    ++ zipKeys "M-" wsKeys [0 ..] (withNthWorkspace S.greedyView)
    ++ zipKeys "M-S-" wsKeys [0 ..] (withNthWorkspace S.shift)
    ++ zipKeys "M-C-S-" wsKeys [0 ..] (withNthWorkspace copy)
  where
    wsKeys = show <$> [1 .. 9 :: Int]

keysSpawnables :: Keybindings
keysSpawnables =
  [ ("M-<Return>", spawn $ C.term C.applications),
    ("M-S-<Return>", spawn $ C.term C.applications ++ " -e tmux"),
    ("M-C-<Return>", spawn $ C.termSmallFont C.applications),
    ("M-C-S-<Return>", spawn $ C.termSmallFont C.applications ++ " -e tmux"),
    ("M-o b", spawn $ C.browser C.applications),
    ("M-o S-b", wrapKbdLayout $ selectBrowserByNameAndSpawn promptTheme),
    ("M-o e", raiseEditor),
    ("M-o r", spawn $ C.term C.applications ++ " -e ranger"),
    ("M-o S-e", spawn "doom +everywhere"),
    ("M-o c", namedScratchpadAction scratchpads "console"),
    ("M-o m", namedScratchpadAction scratchpads "music"),
    ("M-o t", namedScratchpadAction scratchpads "top"),
    ("M-o v", namedScratchpadAction scratchpads "volume"),
    ("M-o s", namedScratchpadAction scratchpads "soundEffects"),
    ("M-o d", namedScratchpadAction scratchpads "discord")
  ]

keysWindows :: Keybindings
keysWindows =
  [ ("M-w k", kill),
    ("M-w S-k", wrapKbdLayout $ confirmPrompt hotPromptTheme "Kill all" killAll),
    ( "M-w C-S-k",
      wrapKbdLayout $
        confirmPrompt hotPromptTheme "Kill others" $
          withOthers killWindow
    ),
    ("M-w d", wrapKbdLayout windowMenu),
    ("M-w g", withDefaultKbdLayout $ windowPrompt promptTheme Goto allWindows),
    ("M-w /", withDefaultKbdLayout $ windowPrompt promptTheme Goto wsWindows),
    ("M-w b", withDefaultKbdLayout $ windowPrompt promptTheme Bring allWindows),
    ("M-w c", toggleCopyToAll),
    ("M-w o", sendMessage Mag.Toggle),
    ("M-w S-c", kill1), -- To remove focused copied window from current workspace
    ("M-w h", withFocused minimizeWindow),
    ("M-w M1-h", withOthers minimizeWindow),
    ("M-w S-h", withLastMinimized maximizeWindowAndFocus),
    ( "M-w C-S-h",
      wrapKbdLayout $ selectMaximizeWindowPrompt $ promptNoCompletion promptTheme -- , ("M-w C-S-h", selectMaximizeWindowGrid)
    ),
    ("M-w r", sendMessage $ Toggle REFLECTX), -- ("M-w r", tryMessageR_ Rotate (Toggle REFLECTX))
    ("M-w t", withFocused $ sendMessage . MergeAll),
    ("M-w S-t", withFocused $ sendMessage . UnMerge),
    ("M-w u", focusUrgent),
    ("M-w m", windows S.focusMaster),
    ("M-w S-m", whenX (swapHybrid True) dwmpromote), -- , ("M-w S-m", dwmpromote)
    ("M-'", onGroup S.focusDown'),
    ("M-;", onGroup S.focusUp'),
    ("M-S-'", windows S.swapDown),
    ("M-S-;", windows S.swapUp),
    ("M-w <Space>", selectWindow def >>= (`whenJust` windows . S.focusWindow)),
    ( "M-w S-<Space>",
      selectWindow def >>= (`whenJust` windows . (S.shiftMaster .) . S.focusWindow)
    ),
    ("M-/", windows S.focusDown),
    ("M-S-/", windows S.focusUp)
  ]
    ++ zipKeys' "M-" vimKeys directions windowGo True
    ++ zipKeys' "M-S-" vimKeys directions windowSwap True
    ++ zipKeys "M-C-" vimKeys directions (sendMessage . pullGroup)
    ++ zipKeys' "M-" arrowKeys directions screenGo True
    ++ zipKeys' "M-S-" arrowKeys directions windowToScreen True
    ++ zipKeys' "M-C-" arrowKeys directions screenSwap True
  where
    directions = [D, U, L, R]
    arrowKeys = ["<D>", "<U>", "<L>", "<R>"]
    vimKeys = ["j", "k", "h", "l"]

keysResize :: Keybindings
keysResize =
  [ ("M-[", tryMessageR_ (ExpandTowards L) Shrink),
    ("M-]", tryMessageR_ (ExpandTowards R) Expand),
    ("M-S-[", tryMessageR_ (ExpandTowards U) MirrorShrink),
    ("M-S-]", tryMessageR_ (ExpandTowards D) MirrorExpand),
    ("M-C-[", tryMessageR_ (ShrinkFrom R) Shrink),
    ("M-C-]", tryMessageR_ (ShrinkFrom L) Expand),
    ("M-S-C-[", tryMessageR_ (ShrinkFrom D) MirrorShrink),
    ("M-S-C-]", tryMessageR_ (ShrinkFrom U) MirrorExpand)
  ]

keysLayout :: XConfig Layout -> Keybindings
keysLayout c =
  [ ("M-<Tab>", sendMessage NextLayout),
    ("M-C-<Tab>", toSubl NextLayout),
    ("M-S-<Tab>", setLayout $ XMonad.layoutHook c),
    ("M-y", withFocused toggleFloat),
    ("M-S-y", sinkAll),
    ("M-S-,", sendMessage $ IncMasterN (-1)),
    ("M-S-.", sendMessage $ IncMasterN 1),
    ( "M-f",
      sequence_ [withFocused $ windows . S.sink, sendMessage $ Toggle NBFULL]
    ),
    -- , ("M-f"          , withFocused toggleFullFloat)
    ("M-S-f", withFocused $ sendMessage . maximizeRestore),
    ("M-t g", toggleGaps),
    ("M-t s", toggleStatusBar),
    ("M-t z", flash' "🌿" >> toggleZen)
  ]

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
      ((modMask .|. shiftMask, button1), dragWindow),
      --- Some touchpad things
      -- Go to next workspace on left scroll
      ((mod4Mask, 7), const nextNonEmptyWS),
      -- Go to previous workspace on right scroll
      ((mod4Mask, 6), const prevNonEmptyWS)
    ]
