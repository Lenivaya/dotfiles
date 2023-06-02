module XMonad.Custom.Layout (
  layoutHook,
  layoutNames,
  CustomTransformers (..),
  toggleZen,
  toggleStatusBar,
  toggleGaps,
) where

import XMonad hiding (layoutHook)
import XMonad.Custom.Theme (tabTheme)
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.RefocusLast
import XMonad.Layout.Accordion
import XMonad.Layout.BinarySpacePartition
import XMonad.Layout.BoringWindows hiding (
  Replace,
 )
import XMonad.Layout.CenteredIfSingle
import XMonad.Layout.Circle
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
import XMonad.Layout.PerWorkspace
import XMonad.Layout.Reflect
import XMonad.Layout.Renamed
import XMonad.Layout.ResizableTile
import XMonad.Layout.ShowWName
import XMonad.Layout.Simplest
import XMonad.Layout.Spacing
import XMonad.Layout.SubLayouts
import XMonad.Layout.Tabbed
import XMonad.Layout.ThreeColumns
import XMonad.Layout.WindowNavigation

data CustomTransformers = GAPS
  deriving (Read, Show, Eq, Typeable)

instance Transformer CustomTransformers Window where
  transform GAPS x k = k (avoidStruts $ applySpacing x) (const x)

applySpacing :: l a -> ModifiedLayout Spacing l a
applySpacing = spacingRaw False (Border 6 6 6 6) True (Border 6 6 6 6) True

layoutNames :: [String]
layoutNames =
  ["BSP", "Circle", "Tall", "ThreeColMid", "OneBig", "Monocle", "Grid"]

bsp = renamed [Replace "BSP"] emptyBSP
tall = renamed [Replace "Tall"] $ ResizableTall 1 (3 / 100) (1 / 2) []
circle = renamed [Replace "Circle"] Circle
threecolmid = renamed [Replace "ThreeColMid"] $ ThreeColMid 1 (3 / 100) (1 / 2)
onebig = renamed [Replace "OneBig"] $ OneBig (3 / 4) (3 / 4)
monocle = renamed [Replace "Monocle"] Full
grid = renamed [Replace "Grid"] $ limitWindows 9 $ Grid (16 / 10)

layoutHook =
  fullscreenFloat
    . fullscreenFull
    . smartBorders
    . boringWindows
    . draggingVisualizer
    . layoutHintsToCenter
    . magnifierOff
    . lessBorders OnlyLayoutFloat
    . mkToggle (single NBFULL)
    . refocusLastLayoutHook
    . avoidStruts
    . applySpacing
    . centeredIfSingle 0.97 0.97
    . mkToggle (single GAPS)
    . mkToggle (single REFLECTX)
    . mkToggle (single REFLECTY)
    . windowNavigation
    . hiddenWindows
    . addTabs shrinkText tabTheme
    . subLayout [] (Simplest ||| Accordion)
    . onWorkspace "Read" (circle ||| onebig)
    . maximize
    . minimize
    $ layouts
  where
    layouts =
      bsp ||| circle ||| tall ||| threecolmid ||| onebig ||| monocle ||| grid

toggleGaps = sendMessage $ Toggle GAPS
toggleStatusBar = sendMessage ToggleStruts
toggleZen =
  sendMessage (Toggle NOBORDERS)
    >> sendMessage ToggleStruts
    >> toggleScreenSpacingEnabled
    >> toggleWindowSpacingEnabled
