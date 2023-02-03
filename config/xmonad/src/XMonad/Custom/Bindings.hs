{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module XMonad.Custom.Bindings
  ( keys
  , rawKeys
  , modMask
  , mouseBindings
  ) where

-- Local
import qualified Data.Map                      as M
import           System.Exit
import           XMonad                  hiding ( keys
                                                , modMask
                                                , mouseBindings
                                                )
import           XMonad.Actions.CopyWindow
import           XMonad.Actions.CycleWS
import           XMonad.Actions.DwmPromote
import           XMonad.Actions.DynamicProjects
import           XMonad.Actions.DynamicWorkspaces
import           XMonad.Actions.EasyMotion      ( selectWindow )
import qualified XMonad.Actions.FlexibleManipulate
                                               as F
import           XMonad.Actions.FloatSnap
import           XMonad.Actions.GridSelect
import           XMonad.Actions.MessageFeedback
import           XMonad.Actions.Minimize
import           XMonad.Actions.MostRecentlyUsed
import           XMonad.Actions.Navigation2D
import           XMonad.Actions.TiledWindowDragging
import           XMonad.Actions.UpdatePointer
import           XMonad.Actions.WindowGo
import           XMonad.Actions.WindowMenu
import           XMonad.Actions.WithAll
import           XMonad.Custom.Layout
import qualified XMonad.Custom.Misc            as C
import           XMonad.Custom.Prompt           ( gridSelectTheme
                                                , hotPromptTheme
                                                , promptNoCompletion
                                                , promptTheme
                                                , promptThemeVim
                                                )
import           XMonad.Custom.Scratchpads
import           XMonad.Custom.Search
import           XMonad.Custom.Workspaces       ( selectBrowserByName )
import           XMonad.Hooks.ManageDocks
import           XMonad.Hooks.UrgencyHook
import           XMonad.Layout.BinarySpacePartition
import           XMonad.Layout.Hidden
import           XMonad.Layout.Maximize
import           XMonad.Layout.MultiToggle
import           XMonad.Layout.MultiToggle.Instances
import           XMonad.Layout.Reflect
import           XMonad.Layout.ResizableTile
import           XMonad.Layout.SubLayouts
import           XMonad.Operations              ( restart )
import           XMonad.Prompt                  ( XPConfig(..) )
import           XMonad.Prompt.ConfirmPrompt
import           XMonad.Prompt.Pass
import           XMonad.Prompt.Shell
import           XMonad.Prompt.Window
import           XMonad.Prompt.Workspace
import qualified XMonad.StackSet               as S
import           XMonad.Util.EZConfig
import           XMonad.Util.NamedScratchpad
                                         hiding ( namedScratchpadFilterOutWorkspace
                                                )
import           XMonad.Util.WorkspaceCompare

modMask :: KeyMask
modMask = mod4Mask

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
    , keysGo
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
  -- , ("M-q r", spawn "xmonad --restart")
  , ("M-q r"   , restart "xmonad" True)
  , ("M-x"     , shellPrompt promptTheme)
  , ("M-S-x"   , spawn (C.appmenu C.applications))
  , ("M-c"     , spawn (C.clipboardSelector C.applications))
  , ("M1-<Tab>", mostRecentlyUsed [xK_Alt_L, xK_Alt_R] xK_Tab)
  ]

keysPass :: XConfig Layout -> [(String, X ())]
keysPass _ =
  [ ("M-p p", passPrompt promptTheme)
  , ("M-p g", passGenerateAndCopyPrompt $ promptNoCompletion promptThemeVim)
  , ("M-p d", passRemovePrompt promptTheme)
  , ("M-p t", passTypePrompt promptTheme)
  ]

keysGo :: XConfig Layout -> [(String, X ())]
keysGo _ = [("M-g s", mySearch)]

keysSystem :: XConfig Layout -> [(String, X ())]
keysSystem _ =
  [ ("<Print>"  , spawn "$XMONAD_CONFIG_DIR/scripts/xshot.sh")
  , ("S-<Print>", spawn "$XMONAD_CONFIG_DIR/scripts/xshot-select.sh")
  , ( "C-S-<Print>"
    , spawn "$XMONAD_CONFIG_DIR/scripts/xshot-select-clipboard.sh"
    )
  , ("M-t c", spawn "$XMONAD_CONFIG_DIR/scripts/toggle-compositor.sh")
  , ("M-t l"  , spawn "zzz")
  , ("M-t S-l", spawn "$XMONAD_CONFIG_DIR/scripts/caffeine")
  ]

keysMedia :: XConfig Layout -> [(String, X ())]
keysMedia _ =
  [ ("<XF86AudioStop>", spawn "mpc stop")
  , ("<XF86AudioPrev>", spawn "mpc prev")
  , ("<XF86AudioNext>", spawn "mpc next")
  ]

keysWorkspaces :: XConfig Layout -> [(String, X ())]
keysWorkspaces _ =
  [ ("M-w S-o"  , switchProjectPrompt promptTheme)
    , ("M-w C-S-o", switchProjectPrompt $ promptNoCompletion promptTheme)
    , ("M-w S-s"  , shiftToProjectPrompt promptTheme)
    , ("M-w S-n"  , renameProjectPrompt hotPromptTheme)
    , ("M-,"      , nextNonEmptyWS)
    , ("M-."      , prevNonEmptyWS)
    , ("M-i"      , toggleWS' ["NSP"])
    , ("M-n", workspacePrompt promptTheme $ windows . S.shift)
    , ("M-w w", gridselectWorkspace gridSelectTheme S.greedyView)
    ]
    ++ zipKeys "M-"     wsKeys [0 ..] (withNthWorkspace S.greedyView)
    ++ zipKeys "M-S-"   wsKeys [0 ..] (withNthWorkspace S.shift)
    ++ zipKeys "M-C-S-" wsKeys [0 ..] (withNthWorkspace copy)
  where wsKeys = map show [1 .. 9 :: Int]

keysSpawnables :: XConfig Layout -> [(String, X ())]
keysSpawnables _ =
  [ ("M-<Return>"  , spawn (C.term C.applications))
  , ("M-S-<Return>", spawn (C.term C.applications ++ " -e tmux"))
  , ("M-o b"       , spawn (C.browser C.applications))
  , ("M-o S-b"     , selectBrowserByName promptTheme)
  , ("M-o e"       , raiseEditor)
    -- Edit some text in emacs
  , ("M-o S-e", spawn "emacsclient --eval '(emacs-everywhere)'")
  , ("M-o c", namedScratchpadAction scratchpads "console")
  , ("M-o m"       , namedScratchpadAction scratchpads "music")
  , ("M-o t"       , namedScratchpadAction scratchpads "top")
  , ("M-o v", namedScratchpadAction scratchpads "volume")
  , ("M-o s", namedScratchpadAction scratchpads "soundEffects")
  , ("M-o d", namedScratchpadAction scratchpads "discord")
  ]

keysWindows :: XConfig Layout -> [(String, X ())]
keysWindows _ =
  [ ("M-w k"  , kill)
    , ("M-w S-k", confirmPrompt hotPromptTheme "Kill all" killAll)
    , ("M-w d"  , windowMenu)
    , ("M-w g", windowPrompt promptTheme Goto allWindows)
    , ("M-w b", windowPrompt promptTheme Bring allWindows)
    , ("M-w c"  , toggleCopyToAll)
    -- To remove focused copied window from current workspace
    , ("M-w S-c", kill1)
    , ("M-w h"  , withFocused minimizeWindow)
    , ("M-w S-h", withLastMinimized maximizeWindowAndFocus)
    , ("M-w r", tryMessageR_ Rotate (Toggle REFLECTX))
    , ("M-w S-r", sendMessage $ Toggle REFLECTX)
    , ("M-w t", withFocused $ sendMessage . MergeAll)
    , ("M-w S-t", withFocused $ sendMessage . UnMerge)
    , ("M-w u"  , focusUrgent)
    , ("M-w m"  , windows S.focusMaster)
    , ("M-w S-m", dwmpromote)
    , ("M-'"    , onGroup S.focusDown')
    , ("M-;"    , onGroup S.focusUp')
    , ("M-S-'"  , windows S.swapDown)
    , ("M-S-;"  , windows S.swapUp)
    , ("M-w s", selectWindow def >>= (`whenJust` windows . S.focusWindow))
    ]
    ++ zipKeys' "M-"   vimKeys directions windowGo   True
    ++ zipKeys' "M-S-" vimKeys directions windowSwap True
    ++ zipKeys "M-C-" vimKeys directions (sendMessage . pullGroup)
    ++ zipKeys' "M-"   arrowKeys directions screenGo       True
    ++ zipKeys' "M-S-" arrowKeys directions windowToScreen True
    ++ zipKeys' "M-C-" arrowKeys directions screenSwap     True
 where
  directions = [D, U, L, R]
  arrowKeys  = ["<D>", "<U>", "<L>", "<R>"]
  vimKeys    = ["j", "k", "h", "l"]

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
  , ( "M-f"
    , sequence_ [withFocused $ windows . S.sink, sendMessage $ Toggle NBFULL]
    )
  , ("M-S-f", withFocused (sendMessage . maximizeRestore))
    -- FIXME Breaks merged tabbed layout
  , ("M-C-g", sendMessage $ Toggle GAPS)
  , ("M-t g", sendMessage ToggleStruts)
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
  [
    -- mod-button1, flexible linear scale
    ((mod4Mask, button1), \w -> focus w >> F.mouseWindow F.discrete w)
  ,
      -- mod-button2, Raise the window to the top of the stack
    ((mod4Mask, button2), \w -> focus w >> windows S.shiftMaster)
  ,
      -- mod-button3, Set the window to floating mode and resize by dragging
    ( (mod4Mask, button3)
    , \w -> focus w >> mouseResizeWindow w >> windows S.shiftMaster
    )
  ,

  -- you may also bind events to the mouse scroll wheel (button4 and button5)
    ((mod4Mask, button4)             , \w -> withFocused minimizeWindow)
  , ((mod4Mask, button5), \w -> withLastMinimized maximizeWindowAndFocus)

  -- Dragging of tiled windows
  , ((modMask .|. shiftMask, button1), dragWindow)
  ,

  --- Some touchpad things
   -- Go to next workspace on left scroll
    ((mod4Mask, 7)                   , const nextNonEmptyWS)
  ,
   -- Go to previous workspace on right scroll
    ((mod4Mask, 6)                   , const prevNonEmptyWS)
  ]
