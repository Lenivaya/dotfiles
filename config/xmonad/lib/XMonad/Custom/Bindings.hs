{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-deprecations #-}

module XMonad.Custom.Bindings (
  myKeys,
  modMask,
  mouseBindings,
) where

import Data.Foldable
import Data.Map qualified as M
import Flow
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
import XMonad.Prompt.RunOrRaise
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

-- import XMonad.Actions.WindowBringer
import XMonad.Actions.CycleRecentWS
import XMonad.Actions.RepeatAction
import XMonad.Custom.Actions.ScratchpadChooser
import XMonad.Custom.Actions.TmuxPrompt

type Keybinding = (String, X ())
type Keybindings = [Keybinding]

type Mousebindings = M.Map (ButtonMask, Button) (Window -> X ())

modMask :: KeyMask
modMask = mod4Mask

{-| Combines modifiers, key sequences, actions, and a function to create keybindings.
Used for creating multiple keybindings with a shared modifier and varying key sequences.
-}
zipKeys
  :: [a]
  -- ^ Modifiers
  -> [[a]]
  -- ^ Key sequences
  -> [t1]
  -- ^ Actions
  -> (t1 -> b)
  -- ^ Binding function
  -> [([a], b)]
  -- ^ Resulting keybindings
zipKeys modifiers keySequences actions bindingFunction =
  zipWith (\keys action -> (modifiers ++ keys, bindingFunction action)) keySequences actions

{-| Similar to zipKeys, but allows an additional parameter for the binding function.
Useful when the binding function requires an extra argument.
-}
zipKeys'
  :: [a]
  -- ^ Modifiers
  -> [[a]]
  -- ^ Key sequences
  -> [t1]
  -- ^ Actions
  -> (t1 -> t2 -> b)
  -- ^ Binding function
  -> t2
  -- ^ Extra parameter
  -> [([a], b)]
  -- ^ Resulting keybindings
zipKeys' modifiers keySequences actions bindingFunction param =
  zipWith
    (\keys action -> (modifiers ++ keys, bindingFunction action param))
    keySequences
    actions

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
  findWorkspace getSortByIndexNonSP Next HiddenNonEmptyWS 1 >>= windows <. S.view
prevNonEmptyWS =
  findWorkspace getSortByIndexNonSP Prev HiddenNonEmptyWS 1 >>= windows <. S.view

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
withOthers f = withWindowSet $ mapM_ f <. others
  where
    others =
      maybe [] integrateOthers
        <. S.stack
        <. S.workspace
        <. S.current

myKeys config = mkKeymap config keys
  where
    keys =
      rememberActions "M-a r" $
        mconcat
          [ keysBase,
            keysPass,
            keysGo,
            keysDo,
            keysSystem,
            keysWorkspaces,
            keysSpawnables,
            keysScreen,
            keysWindows,
            keysLayout config,
            keysResize,
            keysSelect
          ]

flash' = flashText def 0.5

keysBase :: Keybindings
keysBase =
  [ ("M-q q", confirmPrompt (promptNoHistory hotPromptTheme) "Quit XMonad?" $ io exitSuccess),
    -- ("M-q r", spawn "xmonad --recompile" >> restart "xmonad" True),
    -- ("M-q r", restart "xmonad" True),
    ("M-q r", spawn "xmonad --recompile; xmonad --restart"),
    -- ("M-q r", spawn "xmonad --restart"),
    ("M-x", wrapKbdLayout $ shellPrompt $ promptNoCompletion promptTheme),
    ("M-r", wrapKbdLayout $ runOrRaisePrompt promptTheme),
    ("M-S-x", spawn $ C.appmenu C.applications),
    -- , ("M-c", spawn $ C.clipboardSelector C.applications)
    ("M1-<Tab>", mostRecentlyUsed [xK_Alt_L, xK_Alt_R] xK_Tab)
  ]

keysSelect :: Keybindings
keysSelect =
  [ ("M-s k", selectKbdLayout promptTheme),
    ("M-s l", wrapKbdLayout $ selectLayoutByName promptTheme),
    ("M-s S-l", switchToMRUKbdLayout),
    ("M-s o", wrapKbdLayout $ selectScratchpadByName promptTheme)
  ]

keysPass :: Keybindings
keysPass =
  [ ("M-p p", wrapKbdLayout $ passPrompt passPromptTheme),
    ( "M-p g",
      wrapKbdLayout . passGenerateAndCopyPrompt $ promptNoCompletion promptThemeVim
    ),
    ("M-p d", wrapKbdLayout $ passRemovePrompt $ promptNoCompletion passPromptTheme),
    ("M-p t", wrapKbdLayout $ passTypePrompt passPromptTheme),
    ("M-p e", wrapKbdLayout $ passEditPrompt passPromptTheme)
  ]
  where
    passPromptTheme = promptNoHistory promptTheme

keysGo :: Keybindings
keysGo =
  [ -- ("M-g s", mySearch)

    ( "M-g s",
      wrapKbdLayout . selectAndSearchPrompt $ promptNoCompletion promptTheme
    ),
    ("M-g m", wrapKbdLayout . manPrompt $ promptNoCompletion promptTheme),
    ("M-g c", wrapKbdLayout $ calcPrompt (promptNoCompletion promptTheme) ""),
    -- ("M-g t", wrapKbdLayout $ spawn "dmenu-tmux")
    ("M-g t", wrapKbdLayout $ tmuxPrompt promptTheme)
  ]

keysDo :: Keybindings
keysDo =
  [ ("M-d s s", wrapKbdLayout . screenshotPrompt $ promptNoCompletion promptTheme),
    ("M-d s r", wrapKbdLayout . screencastPrompt $ promptNoCompletion promptTheme),
    ("M-d s z", spawn $ C.screenZoomer C.applications),
    ("M-d w c", workspacePrompt promptTheme $ windows . copy),
    -- ("M-d w S-c", copyMenuConfig def {menuArgs = ["-p", "Choose a window to copy:", "-l", "10"]}),
    -- ("M-d w S-c", copyMenuConfig windowBringerConfig),
    -- ("M-d c <Backspace>", spawn "clipcatctl clear"),
    -- ("M-d c l", spawn "clipcat-last"),
    ("M-d c <Backspace>", spawn "greenclip clear"),
    ("M-d m r", spawn "autorandr -c --force")
  ]

keysSystem :: Keybindings
keysSystem =
  [ ("<Print>", screenshot Fullscreen),
    ("S-<Print>", screenshot Select),
    ("C-S-<Print>", screenshot SelectCopyToClipboard),
    ("M-t c", spawn "$XMONAD_CONFIG_DIR/scripts/toggle-compositor.sh"),
    ("M-t l", spawn "zzz"),
    ("M-t S-l", spawn "$XMONAD_CONFIG_DIR/scripts/caffeine"),
    ("M-t C-S-l", spawn "xset dpms force off")
  ]

keysWorkspaces :: Keybindings
keysWorkspaces =
  [ ("M-w S-o", wrapKbdLayout $ switchProjectPrompt $ promptNoHistory promptTheme),
    ( "M-w C-S-o",
      wrapKbdLayout . switchProjectPrompt $ promptNoCompletion promptTheme
    ),
    ("M-w S-s", wrapKbdLayout $ shiftToProjectPrompt $ promptNoHistory promptTheme),
    ("M-w n", wrapKbdLayout $ renameProjectPrompt $ promptNoHistory hotPromptTheme),
    ("M-w <Backspace>", removeWorkspace),
    ( "M-w S-<Backspace>",
      confirmPrompt hotPromptTheme "Kill workspace?" $
        killAll >> removeWorkspace
    ),
    ("M-,", nextNonEmptyWS),
    ("M-.", prevNonEmptyWS),
    ("M-i", toggleWS' ["NSP"]),
    ("M-n", wrapKbdLayout $ workspacePrompt (promptNoHistory promptTheme) $ windows . S.shift),
    ("M-w p", spawn "skippy-xd --paging"),
    ("M-<Tab>", cycleRecentNonEmptyWS [xK_Alt_L, xK_Alt_R, xK_Escape] xK_comma xK_period),
    ("M-w a", currentProject >>= activateProject)
    -- ("M-w w", gridselectWorkspace gridSelectTheme S.greedyView)
    -- ("M-<Tab>", cycleRecentNonEmptyWS [xK_Alt_L, xK_Alt_R, xK_Escape] xK_Tab xK_grave),
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
    ("M-o S-b", wrapKbdLayout $ selectBrowserByNameAndSpawn $ promptNoHistory promptTheme),
    ("M-o e", raiseEditor),
    ("M-o f r", spawn $ C.term C.applications ++ " -e ranger"),
    ("M-o f y", spawn $ C.term C.applications ++ " -e yazi"),
    ("M-o S-e", spawn "doom +everywhere"),
    ("M-o c", namedScratchpadAction scratchpads "console"),
    ("M-s m", namedScratchpadAction scratchpads "music"),
    ("M-o t", namedScratchpadAction scratchpads "top"),
    -- ("M-s S-t", namedScratchpadAction scratchpads "telegram"),
    ("M-o v", namedScratchpadAction scratchpads "volume"),
    ("M-o s", namedScratchpadAction scratchpads "soundEffects"),
    ("M-o d", namedScratchpadAction scratchpads "discord"),
    ("M-s p", namedScratchpadAction scratchpads "cpupower"),
    ("M-s b", namedScratchpadAction scratchpads "bluetooth"),
    ("M-o r", namedScratchpadAction scratchpads "reader"),
    ("M-s g", runOrRaise "gitbutler-tauri" (className =? "gitbutler-tauri"))
  ]

keysScreen :: Keybindings
keysScreen =
  [ ("M-s n", nextScreen),
    ("M-s p", prevScreen)
  ]

keysWindows :: Keybindings
keysWindows =
  [ ("M-w k", kill),
    ("M-w w", spawn "skippy-xd --expose"),
    ( "M-w S-k",
      wrapKbdLayout $
        confirmPrompt
          ( promptNoHistory
              hotPromptTheme
          )
          "Kill all"
          killAll
    ),
    ( "M-w C-S-k",
      wrapKbdLayout $
        confirmPrompt (promptNoHistory hotPromptTheme) "Kill others" $
          withOthers killWindow
    ),
    -- ("M-w d", wrapKbdLayout windowMenu),
    -- ("M-w g", wrapKbdLayout $ windowMultiPrompt (promptNoHistory promptTheme) [(Goto, allWindows), (Goto, wsWindows)]),
    ("M-w g", wrapKbdLayout $ windowPrompt (promptNoHistory promptTheme) Goto allWindows),
    ("M-w /", wrapKbdLayout $ windowPrompt (promptNoHistory promptTheme) Goto wsWindows),
    ("M-w b", wrapKbdLayout $ windowPrompt (promptNoHistory promptTheme) Bring allWindows),
    ( "M-d w S-c",
      wrapKbdLayout $ windowPrompt (promptNoHistory promptTheme) BringCopy allWindows
    ),
    ("M-w c", toggleCopyToAll),
    ("M-w o", sendMessage Mag.Toggle),
    ("M-w S-c", kill1), -- To remove focused copied window from current workspace
    ("M-w d h o", withOthers $ minimizeWindow),
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
    ("M-w C-S-<Space>", selectWindow def >>= (`whenJust` killWindow)),
    ("M-w s h", selectWindow def >>= (`whenJust` minimizeWindow)),
    ("M-/", windows S.focusDown),
    ("M-S-/", windows S.focusUp)
  ]
    ++ zipKeys' "M-" vimKeys directions windowGo True
    ++ zipKeys' "M-S-" vimKeys directions windowSwap True
    ++ zipKeys "M-C-" vimKeys directions (sendMessage . pullGroup)
    ++ zipKeys' "M-" arrowKeys directions screenGo True
    ++ zipKeys' "M-S-" arrowKeys directions windowToScreen True
    ++ zipKeys' "M-C-" arrowKeys directions screenSwap True
    ++ zipKeys' "C-<Space> " vimKeys directions screenSwap True
    ++ zipKeys' "C-<Space> S-" vimKeys directions windowToScreen True
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
  [ -- ("M-d l-n", sendMessage NextLayout),
    -- ("M-d l-s", toSubl NextLayout),
    ("M-d C-S-r", setLayout $ XMonad.layoutHook c),
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
    ("M-t z", toggleZen >> flash' "<~ ZEN ~>")
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
      ((modMask .|. shiftMask, button1), dragWindow)
      --- Some touchpad things
      -- -- Go to next workspace on left scroll
      -- ((mod4Mask, 7), const nextNonEmptyWS),
      -- -- Go to previous workspace on right scroll
      -- ((mod4Mask, 6), const prevNonEmptyWS)
    ]
