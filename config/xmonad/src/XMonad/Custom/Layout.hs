{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}

module XMonad.Custom.Layout
  ( layoutHook
  , selectLayoutByName
  , toggleLayout
  , CustomTransformers(..)
  ) where

-- layout prompt
import           Data.Map                       ( Map )
import qualified Data.Map                      as Map
import           Data.Maybe                     ( fromMaybe )
import           XMonad                  hiding ( float
                                                , layoutHook
                                                , (|||)
                                                )
import           XMonad.Custom.Prompt           ( aListCompFunc )
import qualified XMonad.Custom.Theme           as T
import           XMonad.Hooks.ManageDocks
import           XMonad.Hooks.RefocusLast
import           XMonad.Layout.Accordion
import           XMonad.Layout.BinarySpacePartition
import           XMonad.Layout.BoringWindows
                                         hiding ( Replace )
import           XMonad.Layout.Circle
import           XMonad.Layout.Fullscreen
import           XMonad.Layout.GridVariants
import           XMonad.Layout.Hidden
import           XMonad.Layout.LayoutCombinators
import           XMonad.Layout.LayoutModifier
import           XMonad.Layout.LimitWindows
import           XMonad.Layout.Maximize
import           XMonad.Layout.Minimize
import           XMonad.Layout.MultiToggle
import           XMonad.Layout.MultiToggle.Instances
import           XMonad.Layout.Named
import           XMonad.Layout.NoBorders
import           XMonad.Layout.OneBig
import           XMonad.Layout.PerWorkspace
import           XMonad.Layout.Reflect
import           XMonad.Layout.Renamed
import           XMonad.Layout.ResizableTile
import           XMonad.Layout.ShowWName
import           XMonad.Layout.Simplest
import           XMonad.Layout.Spacing
import           XMonad.Layout.SubLayouts
import           XMonad.Layout.Tabbed
import           XMonad.Layout.ThreeColumns
import           XMonad.Layout.WindowNavigation
import           XMonad.Prompt
import qualified XMonad.StackSet               as Stack
import           XMonad.Util.ExtensibleState   as XState

applySpacing :: l a -> ModifiedLayout Spacing l a
applySpacing = spacingRaw False (Border 6 6 6 6) True (Border 6 6 6 6) True

data CustomTransformers = GAPS
  deriving (Read, Show, Eq, Typeable)

instance Transformer CustomTransformers Window where
  transform GAPS x k = k (avoidStruts $ applySpacing x) (const x)

bsp = named "BSP" $ emptyBSP

tall = named "Tall" $ ResizableTall 1 (3 / 100) (1 / 2) []

circle = named "Circle" $ Circle

threecolmid = named "ThreeColMid" $ ThreeColMid 1 (3 / 100) (1 / 2)

onebig = named "OneBig" $ OneBig (3 / 4) (3 / 4)

monocle = renamed [Replace "Monocle"] $ Full
    -- $ subLayout [] (smartBorders Simplest)

grid =
  renamed [Replace "Grid"]
    $ limitWindows 9
    -- $ subLayout [] (smartBorders Simplest)
    -- $ mySpacing 8
    $ mkToggle (single MIRROR)
    $ Grid (16 / 10)

layoutHook =
  fullscreenFloat
    .   smartBorders
    .   boringWindows
    .   showWName
    $   lessBorders OnlyLayoutFloat
    $   mkToggle (single NBFULL)
    $   refocusLastLayoutHook
    $   avoidStruts
    $   applySpacing
    $   mkToggle (single GAPS)
    $   mkToggle (single REFLECTX)
    $   mkToggle (single REFLECTY)
    $   windowNavigation
    $   hiddenWindows
    $   addTabs shrinkText T.tabTheme
    $   subLayout [] (Simplest ||| Accordion)
    $   onWorkspace "Read" (circle ||| onebig)
    .   maximize
    .   minimize
    $   bsp
    ||| circle
    ||| tall
    ||| threecolmid
    ||| onebig
    ||| monocle
    ||| grid

--------------------------------------------------------------------------------

-- | A data type for the @XPrompt@ class.
data LayoutByName = LayoutByName

instance XPrompt LayoutByName where
  showXPrompt LayoutByName = "Layout: "

--------------------------------------------------------------------------------

-- | Use @Prompt@ to choose a layout.
selectLayoutByName :: XPConfig -> X ()
selectLayoutByName conf = mkXPrompt LayoutByName
                                    conf
                                    (aListCompFunc conf layoutNames)
                                    go
 where
  go :: String -> X ()
  go selected = case lookup selected layoutNames of
    Nothing   -> return ()
    Just name -> sendMessage (JumpToLayout name)

  layoutNames :: [(String, String)]
  layoutNames =
    [ ("BSP"        , "BSP")
    , ("Circle"     , "Circle")
    , ("OneBig"     , "OneBig")
    , ("Tall"       , "Tall")
    , ("ThreeColMid", "ThreeColMid")
    , ("Monocle"    , "Monocle")
    , ("Grid"       , "Grid")
    ]

--------------------------------------------------------------------------------

-- | Keep track of layouts when jumping with 'toggleLayout'.
newtype LayoutHistory = LayoutHistory
  {runLayoutHistory :: Map String String}
  deriving (Typeable)

instance ExtensionClass LayoutHistory where
  initialValue = LayoutHistory Map.empty

--------------------------------------------------------------------------------

-- | Toggle between the current layout and the one given as an argument.
toggleLayout :: String -> X ()
toggleLayout name = do
  winset <- XMonad.gets windowset

  let ws = Stack.workspace . Stack.current $ winset
      wn = Stack.tag ws
      ld = description . Stack.layout $ ws

  if name == ld then restoreLayout wn else rememberAndGo wn ld
 where
    -- Restore the previous workspace.
  restoreLayout :: String -> X ()
  restoreLayout ws = do
    history <- runLayoutHistory <$> XState.get
    let ld = fromMaybe "Auto" (Map.lookup ws history)
    sendMessage (JumpToLayout ld)

  -- Remember the current workspace and jump to the requested one.
  rememberAndGo :: String -> String -> X ()
  rememberAndGo ws current = do
    history <- runLayoutHistory <$> XState.get
    XState.put (LayoutHistory $ Map.insert ws current history)
    sendMessage (JumpToLayout name)
