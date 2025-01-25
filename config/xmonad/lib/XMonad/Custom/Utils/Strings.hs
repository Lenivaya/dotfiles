module XMonad.Custom.Utils.Strings where

import Data.Char (isSpace)

-- | Trims leading and trailing whitespace from a string
trim :: String -> String
trim = f . f
  where
    f = reverse . dropWhile isSpace
