{-# LANGUAGE ImportQualifiedPost #-}

{-|
Module      :  XMonad.Custom.Actions.JumpWorkspaces
Description :  Functions for working with recently used workspaces.
Copyright   :  (c) Danylo Osipchuk, 2025
License     :  BSD3-style
Maintainer  :  danylo.osipchuk@proton.me

Provides functionality for jumping over workspace using vim-like chord sequences.
-}
module XMonad.Custom.Actions.JumpWorkspaces (
  withChordWorkspaceSelection,
) where

import Data.Char (toLower)
import Data.List (intercalate, isPrefixOf)
import Data.Map.Strict qualified as M
import Data.Ratio ((%))
import XMonad
import XMonad.Actions.MostRecentlyUsed (Location (..))
import XMonad.Actions.ShowText
import XMonad.Custom.Actions.RecentWindows
import XMonad.Custom.Utils.ChordKeys
import XMonad.Prelude
import XMonad.Prompt
import XMonad.StackSet qualified as W
import XMonad.Util.ExtensibleConf qualified as XC
import XMonad.Util.ExtensibleState qualified as XS
import XMonad.Util.History (History, erase, event, ledger, origin)

data ChordWorkspacePrompt = ChordWorkspacePrompt

instance XPrompt ChordWorkspacePrompt where
  showXPrompt ChordWorkspacePrompt = "Workspace Chord: "
  commandToComplete _ = id

-- | Represents a workspace with its display information
data WorkspaceInfo = WorkspaceInfo
  { wsId :: WorkspaceId,
    wsName :: String,
    wsChord :: Maybe String
  }

{-| Select workspace using vim-style chord sequence.
Uses vim-like chord sequences for quick selection, falling back to fuzzy search
if the input doesn't match any chord. This version uses alphabetically sorted
workspaces for deterministic chord positions.

Example usage:
> withChordWorkspaceSelection 5 def (windows . W.greedyView)  -- Select from 5 recent workspaces

Parameters:
  * numWorkspaces - Number of recent workspaces to show
  * xpconfig      - XPrompt configuration for appearance
  * action        - Function to apply to selected workspace ID
-}
withChordWorkspaceSelection :: Int -> XPConfig -> (WorkspaceId -> X ()) -> X ()
withChordWorkspaceSelection numWorkspaces xpconfig action = do
  wset <- gets windowset
  let wsIds = map W.tag $ W.workspaces wset -- Get all workspace IDs
      sortedWsIds = sort wsIds -- Sort workspaces alphabetically
  unless (null sortedWsIds) $ processWorkspaceSelection sortedWsIds
  where
    processWorkspaceSelection :: [WorkspaceId] -> X ()
    processWorkspaceSelection wsIds = do
      -- Create workspace info objects with deterministic ordering
      let wsInfos = map (\ws -> WorkspaceInfo ws ws Nothing) wsIds
          -- Generate chords using built-in scheme
          chordMapRaw = generateChords defaultChordScheme wsIds
          chordMap = M.fromList [(w, s) | (s, w) <- M.toList $ fromMaybe M.empty chordMapRaw]
          decoratedInfos = map (addChordInfo chordMap) wsInfos

      showWorkspacePrompt decoratedInfos

    addChordInfo :: M.Map WorkspaceId String -> WorkspaceInfo -> WorkspaceInfo
    addChordInfo chordMap info =
      info {wsChord = M.lookup (wsId info) chordMap}

    showWorkspacePrompt infos = do
      let displayMap = M.fromList [(formatWorkspaceEntry i, wsId i) | i <- infos]
          displayNames = map formatWorkspaceEntry infos

      mkXPrompt
        ChordWorkspacePrompt
        (adjustXPConfig numWorkspaces xpconfig)
        (mkChordCompleter displayNames)
        (selectWorkspace displayMap)

    formatWorkspaceEntry info =
      "[" ++ fromMaybe "?" (wsChord info) ++ "] " ++ wsName info

    mkChordCompleter names input = do
      return $ filter (matchChord input) names
      where
        matchChord "" = const True
        matchChord input = \name ->
          case extractChordFromBracket name of
            Just chord -> input `isPrefixOf` chord
            Nothing -> False

    selectWorkspace displayMap str =
      whenJust (M.lookup str displayMap) action

    adjustXPConfig :: Int -> XPConfig -> XPConfig
    adjustXPConfig num cfg =
      cfg
        { maxComplRows = Just (fromIntegral num),
          maxComplColumns = Just 3,
          searchPredicate = \needle haystack ->
            map toLower needle `isInfixOf` map toLower haystack
        }
