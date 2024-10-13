module XMonad.Custom.Actions.LayoutChooser where

import qualified Data.Map as M
import XMonad
import XMonad.Actions.GridSelect
import XMonad.Custom.Hooks.Layout (layoutMap, layoutNames)
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
    (mkComplFunFromList' conf layoutNames)
    (sendMessage . JumpToLayout . (M.findWithDefault "" `flip` layoutMap))

-- GridSelect
gridChooselayoutTheme :: GSConfig String
gridChooselayoutTheme =
  gridSelectTheme {gs_cellwidth = 500, gs_font = "xft:monospace:size=12"}

selectLayoutGrid :: X ()
selectLayoutGrid =
  gridselect gridChooselayoutTheme (zip layoutNames layoutNames)
    >>= mapM_ (sendMessage . JumpToLayout)
