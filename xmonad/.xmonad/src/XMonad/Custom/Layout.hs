{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}

module XMonad.Custom.Layout
    ( layoutHook
    , CustomTransformers (..)
    ) where

import           XMonad                              hiding (layoutHook)
import           XMonad.Custom.Theme
import           XMonad.Hooks.ManageDocks
import           XMonad.Layout.Accordion
import           XMonad.Layout.BinarySpacePartition
import           XMonad.Layout.Fullscreen
import           XMonad.Layout.Hidden
import           XMonad.Layout.LayoutModifier
import           XMonad.Layout.MultiToggle
import           XMonad.Layout.MultiToggle.Instances
import           XMonad.Layout.NoBorders
import           XMonad.Layout.Reflect
import           XMonad.Layout.Simplest
import           XMonad.Layout.Spacing
import           XMonad.Layout.SubLayouts
import           XMonad.Layout.Tabbed
import           XMonad.Layout.WindowNavigation
import           XMonad.Layout.ResizableTile
import           XMonad.Layout.Gaps
import           XMonad.Layout.IfMax
import           XMonad.Layout.ThreeColumns
import           XMonad.Layout.NoBorders
import           XMonad.Layout.Named
import           XMonad.Layout.ResizableTile
import           XMonad.Layout.LimitWindows (limitWindows, increaseLimit, decreaseLimit)
import           XMonad.Layout.Renamed (renamed, Rename(CutWordsLeft, Replace))
import           XMonad.Layout.OneBig


applySpacing :: l a -> ModifiedLayout Spacing l a
applySpacing = spacingRaw False (Border 6 6 6 6) True (Border 6 6 6 6) True

data CustomTransformers = GAPS
                        deriving (Read, Show, Eq, Typeable)

instance Transformer CustomTransformers Window where
    transform GAPS x k = k (avoidStruts $ applySpacing x) (const x)

data Gaps' = Gaps'
  { u  :: Int
  , d  :: Int
  , x  :: Int
  , x' :: Integer
  }

gs :: Gaps'
gs = Gaps'
  { u  = 30
  , d  = 20
  , x  = 15
  , x' = 15
  }

gapses :: l a -> ModifiedLayout Gaps l a
gapses     = gaps [(U, u gs), (R, x gs), (L, x gs), (D, d gs)]

spacingses :: l a -> ModifiedLayout Spacing l a
spacingses = spacingRaw True (Border      0  (x' gs) (x' gs) (x' gs))
                        True (Border (x' gs) (x' gs) (x' gs) (x' gs))
                        True

space = renamed [Replace "space"] 
        $ limitWindows 4  
        $ spacing 12 
        $ Mirror 
        $ mkToggle (single MIRROR) 
        $ mkToggle (single REFLECTX) 
        $ mkToggle (single REFLECTY) 
        $ OneBig (2/3) (2/3)

oneBig = renamed [Replace "oneBig"]
        $ limitWindows 6  
        $ Mirror 
        $ mkToggle (single MIRROR) 
        $ mkToggle (single REFLECTX) 
        $ mkToggle (single REFLECTY) 
        $ OneBig (5/9) (8/12)

full = named "Fullscreen"
     $ noBorders (fullscreenFull Full)
tall = named "Tall"
     $ IfMax 1 full
     $ gapses
     . spacingses
     $ ResizableTall 1 (2/100) (1/2) []

tcm  = named "Three Columns"
     $ IfMax 1 full
     $ gapses
     $ hiddenWindows
     . spacingses
     $ ThreeColMid 1 (1/10) (1/2)

bsp = named "BSP"
             $ avoidStruts
             $ applySpacing
             $ lessBorders OnlyLayoutFloat
             $ windowNavigation
             $ hiddenWindows
             $ addTabs shrinkText tabTheme
          -- $ mkToggle (single GAPS)
             $ subLayout [] (Simplest ||| Accordion)
             emptyBSP

layoutHook = fullscreenFloat
             . smartBorders
             $ mkToggle (single REFLECTX)
             $ mkToggle (single REFLECTY)
             $ mkToggle (single NBFULL)

             $ bsp 
          ||| tcm
          ||| tall
          ||| oneBig
          ||| space
