{-# LANGUAGE FlexibleInstances #-}

module XMonad.Custom.Hooks.Layout (
  layoutHook,
  layoutNames,
  layoutMap,
  defaultLayout,
  CustomTransformers (..),
  toggleZen,
  toggleStatusBar,
  toggleGaps,
) where

import qualified Data.Map as M
import Data.Ratio ((%))
import XMonad hiding (layoutHook)

import Flow

-- import XMonad.Actions.MouseResize
import XMonad.Custom.Theme (tabTheme)
import XMonad.Custom.Workspaces
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.RefocusLast
import XMonad.Layout.Accordion
import XMonad.Layout.BinarySpacePartition
import XMonad.Layout.BoringWindows hiding (Replace)
import XMonad.Layout.CenterMainFluid
import XMonad.Layout.CenteredIfSingle
import XMonad.Layout.CircleEx
import XMonad.Layout.DraggingVisualizer
import XMonad.Layout.Fullscreen
import XMonad.Layout.GridVariants
import XMonad.Layout.Hidden
import XMonad.Layout.LayoutCombinators
import XMonad.Layout.LayoutHints
import XMonad.Layout.LayoutModifier
import XMonad.Layout.LimitWindows
import XMonad.Layout.Magnifier hiding (Toggle)
import XMonad.Layout.Maximize
import XMonad.Layout.Minimize
import XMonad.Layout.MultiToggle
import XMonad.Layout.MultiToggle.Instances
import XMonad.Layout.NoBorders
import XMonad.Layout.OneBig
import XMonad.Layout.PerScreen
import XMonad.Layout.PerWorkspace
import XMonad.Layout.Reflect
import XMonad.Layout.Renamed
import XMonad.Layout.ResizableTile
import XMonad.Layout.Roledex
import XMonad.Layout.ShowWName
import XMonad.Layout.Simplest
import XMonad.Layout.Spacing
import XMonad.Layout.SubLayouts
import XMonad.Layout.Tabbed
import XMonad.Layout.ThreeColumns
import XMonad.Layout.TwoPane

-- import XMonad.Layout.WindowArranger
import XMonad.Layout.WindowNavigation

data CustomTransformers = GAPS
  deriving (Read, Show, Eq, Typeable)

instance Transformer CustomTransformers Window where
  transform GAPS x k = k (avoidStruts $ applySpacing x) (const x)

applySpacing :: l a -> ModifiedLayout Spacing l a
applySpacing = spacingRaw False (Border 6 6 6 6) True (Border 6 6 6 6) True

setName :: String -> l a -> ModifiedLayout Rename l a
setName n = renamed [Replace n]

rTall :: Int -> Rational -> Rational -> ResizableTall l
rTall m r c = ResizableTall m r c []

bsp = setName "BSP" emptyBSP
tall = setName "Tall" $ rTall 1 (3 % 100) (1 % 2)
circleLayout = setName "Circle" $ magnifiercz' 2 circle
twoPane = TwoPane (3 % 100) (1 % 2)
onebig = setName "OneBig" $ OneBig (3 / 4) (3 / 4)
monocle = setName "Monocle" Full
grid = setName "Grid" $ limitWindows 9 $ Grid (16 / 10)
roledex = Roledex
centerMainFluid = CenterMainFluid 1 (3 / 100) (70 / 100)

hacking =
  setName "Hacking"
    . limitWindows 3
    . magnify 1.3 (NoMaster 3) True
    $ rTall 1 (3 % 100) (13 % 25)

-- threecolmid = setName "ThreeColMid" $ ThreeColMid 1 (3 / 100) (1 / 2)
-- threecol = setName "ThreeCol" $ ThreeCol 1 (3 / 100) (1 / 2)
threeColMid =
  setName "ThreeColMid"
    . magnify 1.2 (NoMaster 4) True
    $ ThreeColMid 1 (3 % 100) (11 % 30)

flex =
  setName "Flex" $
    ifWider smallMonResWidth wideLayout standardLayout
  where
    smallMonResWidth = 1920
    wideLayout =
      ThreeColMid 1 (1 / 20) (1 / 2)
        ||| emptyBSP
    standardLayout =
      rTall 1 (1 / 20) (2 / 3)
        ||| rTall 1 (1 / 20) (1 / 2)

(|||!) (joined, layouts) newLayout =
  (joined ||| newLayout, layouts <> [Layout newLayout])

layoutsStart layout = (layout, [Layout layout])

layoutsInfo =
  layoutsStart flex
    |||! bsp
    |||! hacking
    |||! tall
    |||! twoPane
    |||! threeColMid
    |||! circleLayout
    -- \|||! threecolmid
    -- \|||! threecol
    |||! onebig
    |||! monocle
    |||! grid
    |||! roledex
    |||! centerMainFluid

layouts = fst layoutsInfo
layoutNames = description <$> snd layoutsInfo
layoutMap = M.fromList $ zip layoutNames layoutNames
defaultLayout = head layoutNames

-- fullscreenFloat
-- . fullscreenFull
-- . draggingVisualizer
-- . mouseResize
-- . windowArrange
-- . magnifierOff
-- . onWorkspace wsread (circle ||| flex ||| onebig)
layoutHook =
  maximize
    .> minimize
    .> centeredIfSingle 0.9 0.95
    .> refocusLastLayoutHook
    .> subLayout [] (Simplest ||| Accordion)
    .> addTabs shrinkText tabTheme
    .> windowNavigation
    .> hiddenWindows
    .> applySpacing
    .> avoidStruts
    .> mkToggle (single NBFULL)
    .> mkToggle (single GAPS)
    .> mkToggle (single REFLECTX)
    .> mkToggle (single REFLECTY)
    .> lessBorders OnlyLayoutFloat
    .> magnifierczOff' 1.3
    .> layoutHintsToCenter
    .> boringWindows
    .> smartBorders
    <| layouts

toggleGaps = sendMessage $ Toggle GAPS
toggleStatusBar = sendMessage ToggleStruts
toggleZen =
  sendMessage (Toggle NOBORDERS)
    >> sendMessage ToggleStruts
    >> toggleScreenSpacingEnabled
    >> toggleWindowSpacingEnabled

instance Read (Layout Window) where
  readsPrec _ = readsLayout (Layout layoutHook)
