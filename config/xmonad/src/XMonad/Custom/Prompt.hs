module XMonad.Custom.Prompt
  ( listCompFunc
  , aListCompFunc
  , predicateFunction
  , promptTheme
  , hotPromptTheme
  )
where

import           Data.Char
import           Data.List
import           Data.Ratio
import           XMonad.Prompt
import           XMonad.Prompt.FuzzyMatch
import qualified XMonad.Custom.Theme           as T

promptTheme, hotPromptTheme :: XPConfig
promptTheme = def
  { font              = T.font
  , bgColor           = T.black1
  , fgColor           = T.white1
  , fgHLight          = T.white2
  , bgHLight          = T.black2
  , borderColor       = T.white2
  , promptBorderWidth = T.border
  , height            = T.height
  , position          = CenteredAt { xpCenterY = 3 % 10, xpWidth = 9 % 10 }
  , maxComplRows      = Just 5
  , alwaysHighlight   = True
  , searchPredicate   = fuzzyMatch
  , sorter            = fuzzySort
  , promptKeymap      = emacsLikeXPKeymap
  , autoComplete      = Just 5000
  }

hotPromptTheme = promptTheme { bgColor  = T.black2
                             , fgColor  = T.white2
                             , fgHLight = T.white1
                             , bgHLight = T.black1
                             }

listCompFunc :: XPConfig -> [String] -> String -> IO [String]
listCompFunc c xs s = return (filter (searchPredicate c s) xs)

aListCompFunc :: XPConfig -> [(String, a)] -> String -> IO [String]
aListCompFunc c xs = listCompFunc c (map fst xs)

predicateFunction :: String -> String -> Bool
predicateFunction x y = lc x `isInfixOf` lc y where lc = map toLower
