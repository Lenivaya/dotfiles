{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-deprecations #-}

module XMonad.Custom.Bindings (
  myKeys,
  modMask,
) where

import Data.Foldable
import Data.Map qualified as M
import Flow
import System.Exit
import XMonad (spawn)
import XMonad hiding (
  keys,
  modMask,
 )
import XMonad.Actions.CopyWindow
import XMonad.Actions.CycleRecentWS
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
import XMonad.Actions.RepeatAction
import XMonad.Actions.ShowText
import XMonad.Actions.SwapPromote
import XMonad.Actions.TiledWindowDragging
import XMonad.Actions.UpdatePointer
import XMonad.Actions.WindowGo
import XMonad.Actions.WindowMenu
import XMonad.Actions.WithAll
import XMonad.Custom.Actions.ApplicationChooser
import XMonad.Custom.Actions.Calculator
import XMonad.Custom.Actions.JumpWorkspaces
import XMonad.Custom.Actions.Keyboard
import XMonad.Custom.Actions.LayoutChooser
import XMonad.Custom.Actions.Minimize
import XMonad.Custom.Actions.RecentWindows
import XMonad.Custom.Actions.RecentWorkspaces
import XMonad.Custom.Actions.ScratchpadChooser
import XMonad.Custom.Actions.Screen.Screencast
import XMonad.Custom.Actions.Screen.Screenshot
import XMonad.Custom.Actions.TmuxPrompt
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

type Keybinding = (String, X ())
type Keybindings = [Keybinding]

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
            keysWindows,
            keysWorkspaces,
            keysSystem,
            keysSpawnables,
            keysResize,
            keysDo,
            keysSearch,
            keysLayout config
          ]

flash' = flashText def 0.5

keysBase :: Keybindings
keysBase =
  [ ("M-q q", confirmPrompt hotPromptTheme "Quit XMonad?" $ io exitSuccess),
    ("M-q r", spawn "xmonad --recompile; xmonad --restart"),
    ("M-x", wrapKbdLayout $ shellPrompt $ promptNoCompletion promptTheme),
    ("M-r", wrapKbdLayout $ runOrRaisePrompt promptTheme),
    ("M-S-x", spawn $ C.appmenu C.applications)
    -- ("M1-<Tab>", mostRecentlyUsed [xK_Alt_L, xK_Alt_R] xK_Tab)
  ]

keysSystem :: Keybindings
keysSystem =
  [ ("<Print>", screenshot Fullscreen),
    ("S-<Print>", screenshot Select),
    ("C-S-<Print>", screenshot SelectCopyToClipboard),
    ("M-t S-l", spawn "$XMONAD_CONFIG_DIR/scripts/caffeine"),
    ("M-t t", spawn "darkman toggle")
  ]

keysSpawnables :: Keybindings
keysSpawnables =
  [ ("M-<Return>", spawn $ C.term C.applications),
    ("M-S-<Return>", spawn $ C.term C.applications ++ " -e tmux"),
    ("M-o b", spawn $ C.browser C.applications),
    ("M-o S-b", wrapKbdLayout $ selectBrowserByNameAndSpawn promptTheme),
    -- ("M-o e", raiseEditor),
    ("M-o e", spawn "$TERM --hold -e nvim"),
    ("M-o S-e", wrapKbdLayout $ selectEditorByNameAndSpawn promptTheme),
    ("M-o f f", spawn $ C.term C.applications ++ " -e yazi"),
    ("M-o S-e", spawn "doom +everywhere"),
    ("M-o c", namedScratchpadAction scratchpads "console"),
    ("M-o m", namedScratchpadAction scratchpads "music"),
    ("M-o t", namedScratchpadAction scratchpads "top"),
    -- ("M-s S-t", namedScratchpadAction scratchpads "telegram"),
    ("M-o v", namedScratchpadAction scratchpads "volume"),
    ("M-o s", namedScratchpadAction scratchpads "soundEffects"),
    ("M-o d", namedScratchpadAction scratchpads "discord"),
    ("M-s b", namedScratchpadAction scratchpads "bluetooth"),
    ("M-o r", namedScratchpadAction scratchpads "reader"),
    ("M-o n", namedScratchpadAction scratchpads "notes"),
    ("M-o S-n", namedScratchpadAction scratchpads "notes-nvim"),
    ("M-o g", runOrRaise "gitbutler-tauri" (className =? "gitbutler-tauri"))
  ]

keysDo :: Keybindings
keysDo =
  [ ("M-d s s", wrapKbdLayout . screenshotPrompt $ promptNoCompletion promptTheme),
    ("M-d s r", wrapKbdLayout . screencastPrompt $ promptNoCompletion promptTheme),
    ("M-d s z", spawn $ C.screenZoomer C.applications),
    ("M-d w c", workspacePrompt promptTheme $ windows . copy),
    ("M-d c <Backspace>", spawn "clipcatctl clear"),
    ("M-d l", spawn "dm-tool lock"),
    ("M-d m r", spawn "autorandr -c --force")
  ]

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
  [ ("M-d C-S-r", setLayout $ XMonad.layoutHook c),
    ("M-y", withFocused toggleFloat),
    ("M-S-y", sinkAll),
    ("M-S-,", sendMessage $ IncMasterN (-1)),
    ("M-S-.", sendMessage $ IncMasterN 1),
    ( "M-f",
      sequence_ [withFocused $ windows . S.sink, sendMessage $ Toggle NBFULL]
    ),
    ("M-S-f", withFocused $ sendMessage . maximizeRestore),
    ("M-t g", toggleGaps),
    ("M-t s", toggleStatusBar),
    ("M-t z", toggleZen >> flash' "<~ ZEN ~>")
  ]

keysWindows :: Keybindings
keysWindows =
  [ ("M-w k", kill),
    ("M-w w", spawn "skippy-xd --expose"),
    ( "M-w S-k",
      wrapKbdLayout $
        confirmPrompt
          hotPromptTheme
          "Kill all"
          killAll
    ),
    ( "M-w C-S-k",
      wrapKbdLayout $
        confirmPrompt hotPromptTheme "Kill others" $
          withOthers killWindow
    ),
    ("M-w g", wrapKbdLayout $ windowPrompt promptTheme Goto allWindows),
    ("M-w /", wrapKbdLayout $ windowPrompt promptTheme Goto wsWindows),
    ("M-w b", wrapKbdLayout $ windowPrompt promptTheme Bring allWindows),
    ( "M-d w S-c",
      wrapKbdLayout $ windowPrompt promptTheme BringCopy allWindows
    ),
    ("M-w c", toggleCopyToAll),
    ("M-w o", sendMessage Mag.Toggle),
    ("M-w S-c", kill1), -- To remove focused copied window from current workspace
    ("M-w d h o", withOthers minimizeWindow),
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
    ("M-w <Space>", selectWindow def >>= (`whenJust` windows . S.focusWindow)),
    ( "M-w S-<Space>",
      selectWindow def >>= (`whenJust` windows . (S.shiftMaster .) . S.focusWindow)
    ),
    ("M-w C-S-<Space>", selectWindow def >>= (`whenJust` killWindow)),
    ("M-w s h", selectWindow def >>= (`whenJust` minimizeWindow)),
    ("M-/", windows S.focusDown),
    ("M-S-/", windows S.focusUp),
    ("M-u", onGroup S.focusDown'),
    ("M-i", onGroup S.focusUp'),
    ("M-S-u", windows S.swapDown),
    ("M-S-i", windows S.swapUp),
    ( "M-'",
      wrapKbdLayout $
        withChordSelection
          15
          promptTheme
          (windows . S.focusWindow)
    )
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

keysWorkspaces :: Keybindings
keysWorkspaces =
  [ ("M-p /", wrapKbdLayout $ switchProjectPrompt promptTheme),
    ( "M-p c",
      wrapKbdLayout . switchProjectPrompt $ promptNoCompletion promptTheme
    ),
    ( "M-p s",
      wrapKbdLayout $ gridselectWorkspace gridSelectTheme S.greedyView
    ),
    ("M-p S-s", wrapKbdLayout $ shiftToProjectPrompt promptTheme),
    ("M-p n", wrapKbdLayout $ renameProjectPrompt hotPromptTheme),
    ("M-p <Backspace>", removeWorkspace),
    ( "M-p S-<Backspace>",
      confirmPrompt hotPromptTheme "Kill workspace?" $
        killAll >> removeWorkspace
    ),
    ("M-,", nextNonEmptyWS),
    ("M-.", prevNonEmptyWS),
    ("M-;", toggleWS' ["NSP"]),
    ("M-n", wrapKbdLayout $ workspacePrompt promptTheme $ windows . S.shift),
    ("M-p p", spawn "skippy-xd --paging"),
    ("M-<Tab>", cycleRecentNonEmptyWS [xK_Alt_L, xK_Alt_R, xK_Escape] xK_comma xK_period),
    ( "M-S-;",
      wrapKbdLayout $ withChordWorkspaceSelection 40 promptTheme (windows . S.greedyView)
    )
  ]
    ++ zipKeys "M-" wsKeys [0 ..] (withNthWorkspace S.greedyView)
    ++ zipKeys "M-S-" wsKeys [0 ..] (withNthWorkspace S.shift)
    ++ zipKeys "M-C-S-" wsKeys [0 ..] (withNthWorkspace copy)
  where
    wsKeys = show <$> [1 .. 9 :: Int]

keysSearch :: Keybindings
keysSearch =
  [ ( "M-s e",
      wrapKbdLayout . selectAndSearchPrompt $ promptNoCompletion promptTheme
    ),
    ("M-s m", wrapKbdLayout . manPrompt $ simplestSearch promptTheme),
    ("M-s t", wrapKbdLayout $ tmuxPrompt promptTheme),
    ("M-s p", wrapKbdLayout $ passPrompt passPromptTheme),
    ("M-s w", wrapKbdLayout $ switchProjectPrompt promptTheme),
    ("M-s s", wrapKbdLayout $ windowPrompt promptTheme Goto allWindows),
    ("M-s k", selectKbdLayout promptTheme),
    ("M-s l", wrapKbdLayout $ selectLayoutByName promptTheme),
    ("M-s S-l", switchToMRUKbdLayout),
    ("M-s c", spawn "xcolor | tr -d '[:space:]' | xclip -selection clipboard"),
    ("M-s o", wrapKbdLayout $ selectScratchpadByName promptTheme)
  ]
  where
    passPromptTheme = promptTheme
