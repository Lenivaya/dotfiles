{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

{-|
Module      :  XMonad.Custom.Utils.ChordKeys
Description :  Generate vim-like chord sequences for element selection
Copyright   :  (c) Danylo Osipchuk, 2025
License     :  BSD3-style
Maintainer  :  danylo.osipchuk@proton.me

This module provides utilities for generating chord sequences similar to vim-easymotion
or flash.nvim. It can generate single-key or multi-key sequences based on the number
of elements that need to be labeled.
-}
module XMonad.Custom.Utils.ChordKeys (
  -- * Types
  ChordScheme (..),
  ChordOrderStrategy (..),
  ChordMap,

  -- * Utility Functions
  chordLength,
  maxElementsForScheme,

  -- * Core Functions
  generateChords,
  defaultChordScheme,
  customChordScheme,
  extractChordFromBracket,
) where

import Control.DeepSeq (NFData (..), force)
import Control.Monad (guard)
import Data.Char (isSpace)
import Data.List (sort, sortOn)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as M
import Data.Maybe (fromMaybe)
import Data.Vector qualified as V
import GHC.Generics (Generic)
import XMonad hiding (workspaces)

-- | Configuration for chord generation with validation
data ChordScheme = ChordScheme
  { chordLabels :: {-# UNPACK #-} !String,
    chordOrder :: !ChordOrderStrategy
  }
  deriving (Show, Eq, Generic)

instance NFData ChordScheme

-- | Strategy for ordering chord assignments
data ChordOrderStrategy
  = FrequencyBased
  | AlphaNumeric
  deriving (Eq, Show, Generic)

instance NFData ChordOrderStrategy

{-| Type alias for mapping chord sequences to elements.
Uses 'Map' for O(log n) lookup performance.
-}
type ChordMap a = Map String a

{-| Default chord scheme using home row and common keys.
    Optimized for QWERTY keyboard layout with frequency-based ordering.
-}
defaultChordScheme :: ChordScheme
defaultChordScheme =
  ChordScheme
    { chordLabels = "asdfjkl;ghqwertyuiopzxcvbnm",
      chordOrder = FrequencyBased
    }

{-| Create a custom chord scheme with specified labels and ordering strategy.

Example:
> customChordScheme "123456789" AlphaNumeric  -- Numeric-only chords
> customChordScheme "jkl;asdf" FrequencyBased -- Home row only
-}
customChordScheme :: String -> ChordOrderStrategy -> ChordScheme
customChordScheme = ChordScheme

{-| Calculate minimum chord length needed to represent n elements with given number of labels.

Parameters:
  * numLabels - Number of available single characters
  * numElements - Number of elements that need unique chords

Returns: Required chord length (1, 2, or 3) or Nothing if impossible

Properties:
  * Time complexity: O(1)
  * Space complexity: O(1)

Examples:

>>> chordLength 26 10   -- Returns Just 1 (single char enough)
>>> chordLength 26 30   -- Returns Just 2 (need two chars)
>>> chordLength 26 700  -- Returns Just 3 (need three chars)
>>> chordLength 26 20000 -- Returns Nothing (too many elements)
-}
chordLength :: Int -> Int -> Maybe Int
chordLength numLabels numElements
  | numLabels <= 0 || numElements <= 0 = Nothing
  | numElements <= numLabels = Just 1
  | numElements <= numLabels ^ 2 = Just 2
  | numElements <= numLabels ^ 3 = Just 3
  | otherwise = Nothing

{-| Calculate maximum number of elements possible with a scheme.
This is the sum of all possible combinations with 1, 2, or 3 characters.

Time complexity: O(1) after computing length once
-}
maxElementsForScheme :: ChordScheme -> Int
maxElementsForScheme ChordScheme {chordLabels} =
  let n = length chordLabels
  in  n + n ^ 2 + n ^ 3

{-| Generate chord sequences for a list of elements.
    Returns Nothing if the number of elements exceeds maximum possible chords.

Parameters:
  * scheme - Configuration for chord generation
  * elements - List of elements to generate chords for

Returns: Map from elements to their chord sequences or Nothing if impossible

Example:
> generateChords defaultChordScheme ["win1", "win2"]        -- Single char chords
> generateChords defaultChordScheme ["w1".."w30"]          -- Mix of single and double
> generateChords (customChordScheme "123" AlphaNumeric) xs -- Numeric chords
-}

{-| Generate chord sequences for a list of elements.
    Returns Nothing if the number of elements exceeds maximum possible chords.

    Time complexity: O(n log n) where n is the number of elements
    Space complexity: O(n)
-}
generateChords :: (Ord a, NFData a) => ChordScheme -> [a] -> Maybe (ChordMap a)
generateChords scheme@ChordScheme {chordLabels, chordOrder} elements = do
  let numElements = length elements
      maxElems = maxElementsForScheme scheme
      numLabels = length chordLabels

  -- Early return if we have too many elements
  guard (numElements <= maxElems)

  -- Get required chord length
  requiredLength <- chordLength numLabels numElements

  -- Generate and assign chords
  let
    -- Generate chords of the required length efficiently
    baseChords = generateChordsOfLength chordLabels requiredLength

    -- Apply ordering strategy
    sortedChords = take numElements $ case chordOrder of
      FrequencyBased -> optimizeChords baseChords
      AlphaNumeric -> sort baseChords

    -- Create the final mapping
    result = M.fromList $ zip sortedChords elements

  -- Force evaluation to avoid space leaks
  return $! force result
  where
    -- Optimize chord assignment based on character frequency
    optimizeChords :: [String] -> [String]
    optimizeChords = sortOn chordCost

    -- Calculate cost of a chord (lower is better)
    chordCost :: String -> Int
    chordCost = sum . map charCost

    -- Calculate cost of a single character
    charCost :: Char -> Int
    charCost c = fromMaybe 99 $ M.lookup c costMap

    -- Precompute cost map (lower index = lower cost)
    costMap :: Map Char Int
    costMap = M.fromList $ zip chordLabels [1 ..]

{-| Generate all possible chord combinations of a specific length.
Uses list comprehensions for clarity and efficiency.

Time complexity: O(n^k) where n is the number of labels and k is the chord length
Space complexity: O(n^k)
-}
generateChordsOfLength :: String -> Int -> [String]
generateChordsOfLength labels 1 = map (: []) labels
generateChordsOfLength labels 2 = [[c1, c2] | c1 <- labels, c2 <- labels]
generateChordsOfLength labels 3 = [[c1, c2, c3] | c1 <- labels, c2 <- labels, c3 <- labels]
generateChordsOfLength _ n = error $ "Unsupported chord length: " ++ show n ++ ". Only lengths 1-3 are supported."

{-| Extract chord from bracketed string like "Window [as]".
Returns the content between brackets, trimmed of whitespace.

>>> extractChordFromBracket "Window [as]"  -- Returns Just "as"
>>> extractChordFromBracket "No brackets" -- Returns Nothing
>>> extractChordFromBracket "Window [ jk ]" -- Returns Just "jk"
-}
extractChordFromBracket :: String -> Maybe String
extractChordFromBracket str = case break (== '[') str of
  (_, '[' : rest) -> case break (== ']') rest of
    (chord, ']' : _) -> Just $ trim chord
    _ -> Nothing -- Missing closing bracket
  _ -> Nothing -- No opening bracket
  where
    -- Trim whitespace from both ends of a string
    trim :: String -> String
    trim = dropWhile isSpace . reverse . dropWhile isSpace . reverse
