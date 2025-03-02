{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE ScopedTypeVariables #-}

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
import Data.List (sort, sortOn)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as M
import Data.Maybe (fromMaybe)
import Data.Vector.Unboxed qualified as VU
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

-- | Type alias for mapping chord sequences to elements
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

Example:
> chordLength 26 10   -- Returns Just 1 (single char enough)
> chordLength 26 30   -- Returns Just 2 (need two chars)
> chordLength 26 700  -- Returns Just 3 (need three chars)
-}
chordLength :: Int -> Int -> Maybe Int
chordLength numLabels numElements
  | numLabels <= 0 = Nothing
  | numElements <= 0 = Nothing
  | numElements <= numLabels = Just 1
  | numElements <= numLabels * numLabels = Just 2
  | numElements <= numLabels * numLabels * numLabels = Just 3
  | otherwise = Nothing

-- | Calculate maximum number of elements possible with a scheme
maxElementsForScheme :: ChordScheme -> Int
maxElementsForScheme ChordScheme {chordLabels} =
  let n = length chordLabels
  in  n + n * n + n * n * n

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
generateChords :: (Ord a, NFData a) => ChordScheme -> [a] -> Maybe (ChordMap a)
generateChords scheme@ChordScheme {chordLabels, chordOrder} elements = do
  let numElements = length elements
      maxElems = maxElementsForScheme scheme

  if numElements > maxElems
    then Nothing
    else Just $ force $ M.fromList $ zip sortedChords elements
  where
    numElements = length elements
    labels = chordLabels
    numLabels = length labels

    -- Get required chord length
    requiredLength =
      fromMaybe (error "Unsupported chord length") $
        chordLength numLabels numElements

    -- Generate chords of the required length efficiently
    baseChords = generateChordsOfLength labels requiredLength

    -- Apply ordering strategy
    sortedChords = take numElements $ case chordOrder of
      FrequencyBased -> optimizeChords baseChords
      AlphaNumeric -> sort baseChords

    -- Optimize chord assignment based on character frequency
    optimizeChords = sortOn chordCost
      where
        chordCost :: String -> Int
        chordCost = sum . map charCost

        charCost :: Char -> Int
        charCost c = fromMaybe 99 $ M.lookup c costMap

        -- Precompute cost map (lower index = lower cost)
        costMap = M.fromList $ zip chordLabels [1 ..]

-- | Generate all possible chord combinations of a specific length
generateChordsOfLength :: String -> Int -> [String]
generateChordsOfLength labels 1 = map (: []) labels
generateChordsOfLength labels 2 = [[c1, c2] | c1 <- labels, c2 <- labels]
generateChordsOfLength labels 3 = [[c1, c2, c3] | c1 <- labels, c2 <- labels, c3 <- labels]
generateChordsOfLength _ _ = error "Unsupported chord length"

-- | Extract chord from bracketed string
extractChordFromBracket :: String -> Maybe String
extractChordFromBracket str = case break (== '[') str of
  (_, '[' : rest) -> case break (== ']') rest of
    (chord, _) -> Just $ dropWhile (== ' ') chord
  _ -> Nothing
