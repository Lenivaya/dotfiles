module XMonad.Custom.Actions.LayoutChooser where

import Data.Foldable
import XMonad
import XMonad.Actions.GridSelect
import XMonad.Custom.Hooks.Layout (layoutNames)
import XMonad.Custom.Prompt
import XMonad.Prompt

-- Prompt
data LayoutByName = LayoutByName

instance XPrompt LayoutByName where
  showXPrompt _ = "Layout: "

selectLayoutByName :: XPConfig -> X ()
selectLayoutByName conf =
  mkXPrompt
    LayoutByName
    conf
    (listCompFunc conf layoutNames)
    go
  where
    go selected =
      forM_
        (lookup selected $ zip layoutNames layoutNames)
        (sendMessage . JumpToLayout)

-- GridSelect
gridChooselayoutTheme :: GSConfig String
gridChooselayoutTheme =
  gridSelectTheme {gs_cellwidth = 500, gs_font = "xft:monospace:size=12"}

selectLayoutGrid :: X ()
selectLayoutGrid =
  gridselect gridChooselayoutTheme layouts
    >>= mapM_ (sendMessage . JumpToLayout)
  where
    layouts = zip layoutNames layoutNames
