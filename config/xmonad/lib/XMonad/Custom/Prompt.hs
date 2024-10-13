module XMonad.Custom.Prompt (
  listCompFunc,
  aListCompFunc,
  predicateFunction,
  promptTheme,
  hotPromptTheme,
  promptThemeVim,
  promptNoCompletion,
  promptNoHistory,
  gridSelectTheme,
  helpPromptConfig,
) where

import Data.Char
import Data.List
import Data.Ratio
import XMonad
import XMonad.Actions.GridSelect
import XMonad.Actions.ShowText
import XMonad.Custom.Theme qualified as T
import XMonad.Prompt
import XMonad.Prompt.FuzzyMatch
import qualified Data.Map.Strict as M

promptNoHistory :: XPConfig -> XPConfig
promptNoHistory ptheme = ptheme {historyFilter = const [], historySize = 0}

promptNoCompletion :: XPConfig -> XPConfig
promptNoCompletion ptheme = promptNoHistory ptheme {autoComplete = Nothing}

promptTheme, hotPromptTheme, promptThemeVim :: XPConfig
promptTheme =
  def
    { font = T.font
    , bgColor = T.black1
    , fgColor = T.white1
    , fgHLight = T.white2
    , bgHLight = T.black2
    , borderColor = T.white2
    , promptBorderWidth = T.border
    , height = T.height
    , -- , position             = CenteredAt { xpCenterY = 3 % 10, xpWidth = 9 % 10 }
      position = CenteredAt {xpCenterY = 3 % 10, xpWidth = 1 % 2}
    , maxComplRows = Just 5
    , alwaysHighlight = True
    , historyFilter = deleteAllDuplicates
    , searchPredicate = fuzzyMatch
    , historySize = 256
    , sorter = fuzzySort
    , complCaseSensitivity = CaseInSensitive
    , promptKeymap = emacsLikeXPKeymap
    , autoComplete = (5 `ms`)
    }

promptThemeVim = promptTheme {promptKeymap = vimLikeXPKeymap}

hotPromptTheme =
  promptNoCompletion
    promptTheme
      { bgColor = T.black2
      , fgColor = T.white2
      , fgHLight = T.white1
      , bgHLight = T.black1
      }

colorizer :: a -> Bool -> X (String, String)
colorizer _ isFg = do
  fBC <- asks (focusedBorderColor . config)
  nBC <- asks (normalBorderColor . config)
  return $ if isFg then (fBC, nBC) else (nBC, fBC)

{-| Express the given time in milliseconds as a time in microseconds,
 ready for consumption by @autoComplete@.
-}
ms :: Int -> Maybe Int
ms = Just . (* 10 ^ (4 :: Int))

-- gridSelectTheme :: GSConfig a
-- , gs_cellheight = 30
-- , gs_cellwidth  = 100
gridSelectTheme = (buildDefaultGSConfig colorizer) {gs_font = T.font}

listCompFunc :: XPConfig -> [String] -> String -> IO [String]
listCompFunc c xs s = return $ filter (searchPredicate c s) xs

aListCompFunc :: XPConfig -> [(String, a)] -> String -> IO [String]
aListCompFunc c xs s = return $ map fst $ filter (searchPredicate c s . fst) xs

predicateFunction :: String -> String -> Bool
predicateFunction x y = lc x `isInfixOf` lc y where lc = map toLower

helpPromptConfig :: ShowTextConfig
helpPromptConfig = def {st_font = "xft:monospace:size=12"}
