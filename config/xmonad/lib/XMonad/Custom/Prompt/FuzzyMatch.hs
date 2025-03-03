--------------------------------------------------------------------------------
{-# LANGUAGE BangPatterns #-}

{-|
Module      : XMonad.Custom.Prompt.FuzzyMatch
Description : Fast fuzzy matching for XMonad prompts using Smith-Waterman algorithm
Copyright   : (C) 2025 Danylo Osipchuk
License     : MIT

Maintainer  : Danylo Osipchuk <danylo.osipchuk@proton.me>
Stability   : stable
Portability : portable

A module for fuzzy completion matching in prompts akin to emacs ido mode.
Uses the Smith-Waterman algorithm from Text.FuzzyFind for intelligent matching.
-}
module XMonad.Custom.Prompt.FuzzyMatch (
  -- * Core Functions
  fuzzyMatch,
  fuzzySort,
) where

import Data.Function (on)
import Data.List (sortBy)
import Data.Maybe (isJust)
import Text.FuzzyFind (bestMatch, score)
import XMonad.Prelude

{- $usage
This module offers advanced fuzzy matching capabilities for XMonad prompts,
powered by the Smith-Waterman algorithm from Text.FuzzyFind.

'fuzzyMatch' uses intelligent fuzzy matching with the following features:
- Space-separated pattern support (like fzf)
- Contiguous character matches
- Word boundary awareness
- CamelCase boundary detection
- Position-aware matching
- Case sensitivity control (uppercase = exact, lowercase = fuzzy)

'fuzzySort' provides smart sorting of matches considering:
- Match quality scores
- Word/CamelCase boundary matches
- Contiguous vs scattered matches
- Position of matches
- Combined multi-pattern scores

Example usage in XPrompt:

> import XMonad.Prompt
> import XMonad.Prompt.Window ( windowPrompt )
> import XMonad.Prompt.FuzzyMatch
>
> myXPConfig = def { searchPredicate = fuzzyMatch
>                  , sorter          = fuzzySort
>                  }
>
> , ((modm .|. shiftMask, xK_g), windowPrompt myXPConfig Goto allWindows)

For detailed instructions on editing key bindings, see
<https://xmonad.org/TUTORIAL.html#customizing-xmonad the tutorial>.
-}

{-| Returns True if the query string fuzzy matches the target string.
Space-separated patterns are supported - all must match.
-}
fuzzyMatch :: String -> String -> Bool
fuzzyMatch "" = const True -- Empty query matches everything
fuzzyMatch query = \str -> all (`matches` str) (words query)
  where
    matches = (isJust .) . bestMatch

{-| Sort strings by their fuzzy match scores.
Higher scores appear first in the result.
-}
fuzzySort :: String -> [String] -> [String]
fuzzySort "" = id -- Empty query preserves order
fuzzySort query = map snd . sortBy (flip compare `on` fst) . map scoreString
  where
    patterns = words query
    scoreString str = (getScore str, str)
    getScore str = sum [maybe 0 score (bestMatch pat str) | pat <- patterns]
