module XMonad.Custom.Prompt
  ( listCompFunc
  , aListCompFunc
  , predicateFunction
  , promptTheme
  , hotPromptTheme
  , promptThemeVim
  , promptNoCompletion
  , gridSelectTheme
  ) where

import           Data.Char
import           Data.List
import           Data.Ratio
import           XMonad
import           XMonad.Actions.GridSelect
import qualified XMonad.Custom.Theme           as T
import           XMonad.Prompt
import           XMonad.Prompt.FuzzyMatch


promptNoCompletion :: XPConfig -> XPConfig
promptNoCompletion ptheme = ptheme { autoComplete = Nothing }

promptTheme, hotPromptTheme, promptThemeVim :: XPConfig
promptTheme = def
  { font                 = T.font
  , bgColor              = T.black1
  , fgColor              = T.white1
  , fgHLight             = T.white2
  , bgHLight             = T.black2
  , borderColor          = T.white2
  , promptBorderWidth    = T.border
  , height               = T.height
  , position             = CenteredAt { xpCenterY = 3 % 10, xpWidth = 9 % 10 }
  , maxComplRows         = Just 5
  , alwaysHighlight      = True
  , searchPredicate      = fuzzyMatch
  , sorter               = fuzzySort
  , complCaseSensitivity = CaseInSensitive
  , promptKeymap         = emacsLikeXPKeymap
  , autoComplete         = Just 15000
  }

promptThemeVim = promptTheme { promptKeymap = vimLikeXPKeymap }

hotPromptTheme = promptNoCompletion promptTheme { bgColor      = T.black2
                                                , fgColor      = T.white2
                                                , fgHLight     = T.white1
                                                , bgHLight     = T.black1
                                                , autoComplete = Nothing
                                                }

colorizer :: a -> Bool -> X (String, String)
colorizer _ isFg = do
  fBC <- asks (focusedBorderColor . config)
  nBC <- asks (normalBorderColor . config)
  return $ if isFg then (fBC, nBC) else (nBC, fBC)

-- gridSelectTheme :: GSConfig a
gridSelectTheme = (buildDefaultGSConfig colorizer) { gs_font = T.font }
  -- , gs_cellheight = 30
  -- , gs_cellwidth  = 100

listCompFunc :: XPConfig -> [String] -> String -> IO [String]
listCompFunc c xs s = return (filter (searchPredicate c s) xs)

aListCompFunc :: XPConfig -> [(String, a)] -> String -> IO [String]
aListCompFunc c xs = listCompFunc c (map fst xs)

predicateFunction :: String -> String -> Bool
predicateFunction x y = lc x `isInfixOf` lc y where lc = map toLower
