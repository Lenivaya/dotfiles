--------------------------------------------------------------------------------
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TupleSections #-}

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
Case sensitivity follows Text.FuzzyFind's rules: lowercase letters in the query
match case-insensitively, uppercase letters match case-sensitively.
-}
module XMonad.Custom.Prompt.FuzzyMatch (
  -- * Core Functions
  fuzzyMatch,
  fuzzySort,
) where

import Data.Char (toLower)
import Data.Function (on)
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HM
import Data.List (sortBy)
import Data.Maybe (fromMaybe, isJust)
import Data.Vector (Vector)
import Data.Vector qualified as V
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
- Case sensitivity control (lowercase = case-insensitive, uppercase = case-sensitive)

'fuzzySort' provides smart sorting of matches considering:
- Match quality scores
- Word/CamelCase boundary matches
- Contiguous vs scattered matches
- Position of matches
- Combined multi-pattern scores
- Using Text.FuzzyFind's case sensitivity rules

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

-- | Cache for pattern matching results to avoid redundant calculations
type MatchCache = HashMap (String, String) Bool

-- | Cache for score calculations to avoid redundant calculations
type ScoreCache = HashMap (String, String) Int

{-| Returns True if the query string fuzzy matches the target string.
Space-separated patterns are supported - all must match.
Optimized with memoization to avoid redundant calculations.
Case sensitivity follows Text.FuzzyFind's rules: lowercase letters in the query
match case-insensitively, uppercase letters match case-sensitively.
-}
fuzzyMatch :: String -> String -> Bool
fuzzyMatch "" = const True -- Empty query matches everything
fuzzyMatch query = \str ->
  let !patterns = V.fromList $ words query
      !cache = HM.empty
  in  matchAllPatterns patterns str cache
  where
    -- Check if all patterns match the string
    matchAllPatterns :: Vector String -> String -> MatchCache -> Bool
    matchAllPatterns !patterns !str !cache =
      V.all (\pat -> lookupMatch pat str cache) patterns

    -- Check if a single pattern matches, using cache
    lookupMatch :: String -> String -> MatchCache -> Bool
    lookupMatch !pat !str !cache =
      let key = (pat, str)
      in  case HM.lookup key cache of
            Just result -> result
            Nothing -> isJust (bestMatch pat str)

{-| Sort strings by their fuzzy match scores.
Higher scores appear first in the result.
Optimized with memoization and strict evaluation.
Case sensitivity follows Text.FuzzyFind's rules: lowercase letters in the query
match case-insensitively, uppercase letters match case-sensitively.
-}
fuzzySort :: String -> [String] -> [String]
fuzzySort "" = id -- Empty query preserves order
fuzzySort query =
  let !patterns = V.fromList $ words query
  in  \strs ->
        let !cache = HM.empty
            -- Map original strings to (score, original) pairs
            !scoredStrs =
              map
                ( \str ->
                    (scoreString patterns str cache, str)
                )
                strs
        in  map snd $ sortBy (flip compare `on` fst) scoredStrs
  where
    -- Calculate score for a string against all patterns
    scoreString :: Vector String -> String -> ScoreCache -> Int
    scoreString !patterns !str !cache =
      V.sum $ V.map (\pat -> getPatternScore pat str cache) patterns

    -- Get score for a single pattern, using cache
    getPatternScore :: String -> String -> ScoreCache -> Int
    getPatternScore !pat !str !cache =
      let key = (pat, str)
      in  case HM.lookup key cache of
            Just result -> result
            Nothing -> maybe 0 score (bestMatch pat str)
