{-# LANGUAGE ImportQualifiedPost #-}

module XMonad.Custom.Prompt (
  aListCompFunc,
  promptTheme,
  hotPromptTheme,
  promptThemeVim,
  promptNoCompletion,
  promptNoHistory,
  gridSelectTheme,
  helpPromptConfig,
) where

import Control.Arrow ((***))
import Data.Char
import Data.List
import Data.Map.Strict qualified as M
import Data.Ratio
import XMonad
import XMonad.Actions.GridSelect
import XMonad.Actions.ShowText
import XMonad.Custom.Theme qualified as T
import XMonad.Prompt
import XMonad.Prompt.FuzzyMatch

promptNoHistory :: XPConfig -> XPConfig
promptNoHistory ptheme = ptheme {historyFilter = const [], historySize = 0}

promptNoCompletion :: XPConfig -> XPConfig
promptNoCompletion ptheme = promptNoHistory ptheme {autoComplete = Nothing}

promptTheme, hotPromptTheme, promptThemeVim :: XPConfig
promptTheme =
  def
    { font = T.font,
      bgColor = T.black1,
      fgColor = T.white1,
      fgHLight = T.white2,
      bgHLight = T.black2,
      borderColor = T.white2,
      promptBorderWidth = T.border,
      height = T.height,
      position = CenteredAt (3 % 10) (2 % 5),
      maxComplRows = Just 10,
      maxComplColumns = Just 3,
      alwaysHighlight = True,
      -- , historyFilter = deleteAllDuplicates
      historyFilter = const [], -- fuck history.
      -- , historySize = 256
      historySize = 0,
      searchPredicate = fuzzyMatch,
      sorter = fuzzySort,
      complCaseSensitivity = CaseInSensitive,
      promptKeymap = emacsLikeXPKeymap,
      autoComplete = ms 5
    }
promptThemeVim = promptTheme {promptKeymap = vimLikeXPKeymap}
hotPromptTheme =
  promptNoCompletion $
    promptTheme
      { bgColor = T.black2,
        fgColor = T.white2,
        fgHLight = T.white1,
        bgHLight = T.black1
      }

colorizer :: a -> Bool -> X (String, String)
colorizer _ isFg = do
  fBC <- asks (focusedBorderColor . config)
  nBC <- asks (normalBorderColor . config)
  pure $ if isFg then (fBC, nBC) else (nBC, fBC)

{-| Express the given time in milliseconds as a time in microseconds,
 ready for consumption by @autoComplete@.
-}
ms :: Int -> Maybe Int
ms = Just . (* 10 ^ (4 :: Int))

gridSelectTheme :: GSConfig a
gridSelectTheme = (buildDefaultGSConfig colorizer) {gs_font = T.font}

-- | Optimized completion function for filtering tuples by first element
aListCompFunc :: XPConfig -> [(String, a)] -> String -> IO [String]
aListCompFunc c xs s = pure $! [x | (x, _) <- xs, searchPredicate c s x]

helpPromptConfig :: ShowTextConfig
helpPromptConfig = def {st_font = "xft:monospace:size=12"}
