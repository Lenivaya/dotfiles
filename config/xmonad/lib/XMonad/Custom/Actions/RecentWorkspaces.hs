{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost #-}

{-|
Module      :  XMonad.Custom.Actions.RecentWorkspaces
Description :  Functions for working with recently used workspaces.
Copyright   :  (c) Danylo Osipchuk, 2025
License     :  BSD3-style
Maintainer  :  danylo.osipchuk@proton.me

Provides functionality for chord-based selection of recently used workspaces.
-}
module XMonad.Custom.Actions.RecentWorkspaces (
  -- * Configuration
  configureRecentWorkspaces,

  -- * Workspace Selection
  withChordWorkspaceSelectionFrecency,
  showRecentWorkspaces,
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

-- --< Interface >-- {{{

{- $usage
'configureRecentWorkspaces' must be applied to your config:

> main :: IO ()
> main = xmonad . configureRecentWorkspaces . ... $ def

Then you can use the functions in your keybindings:

> , ((modm, xK_w), showRecentWorkspaces 5 0.5)        -- Show 5 recent workspaces for 0.5 seconds
> , ((modm, xK_Tab), withChordWorkspaceSelectionFrecency 5    -- Select from 5 recent workspaces
>                      def (windows . W.greedyView))
-}

{-| Configure xmonad to support recent workspaces tracking
Now we just need to ensure RecentWindows is configured since we use its state
-}
configureRecentWorkspaces :: XConfig l -> XConfig l
configureRecentWorkspaces = XC.once f (RecentWorkspaces ())
  where
    f cnf = cnf -- No additional configuration needed, we use RecentWindows state
    -- }}}

-- | Configuration type for Recent Workspaces functionality
newtype RecentWorkspaces = RecentWorkspaces () deriving (Semigroup)

-- | Just use RecentWindowsState for workspace history tracking
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

-- | Common XPrompt configuration adjustments
adjustXPConfig :: Int -> XPConfig -> XPConfig
adjustXPConfig num cfg =
  cfg
    { maxComplRows = Just (fromIntegral num),
      maxComplColumns = Just 1,
      searchPredicate = \needle haystack ->
        map toLower needle `isInfixOf` map toLower haystack
    }

{-| Select workspace using vim-style chord sequence.
Uses vim-like chord sequences for quick selection, falling back to fuzzy search
if the input doesn't match any chord.

Example usage:
> withChordWorkspaceSelectionFrecency 5 def (windows . W.greedyView)  -- Select from 5 recent workspaces

Parameters:
  * numWorkspaces - Number of recent workspaces to show
  * xpconfig      - XPrompt configuration for appearance
  * action        - Function to apply to selected workspace ID
-}
withChordWorkspaceSelectionFrecency :: Int -> XPConfig -> (WorkspaceId -> X ()) -> X ()
withChordWorkspaceSelectionFrecency numWorkspaces xpconfig action = do
  wset <- gets windowset
  wsIds <- getRecentWorkspaces numWorkspaces
  unless (null wsIds) $ processWorkspaceSelection wsIds
  where
    processWorkspaceSelection :: [WorkspaceId] -> X ()
    processWorkspaceSelection wsIds = do
      -- Create workspace info objects
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

{-| Show N most recent workspaces on screen with their chord bindings.
Workspaces are displayed with their chord bindings in brackets.

Example usage:
> showRecentWorkspaces 5 0.5  -- Show 5 most recent workspaces for 0.5 seconds
-}
showRecentWorkspaces :: Int -> Double -> X ()
showRecentWorkspaces numWorkspaces displayTime = do
  wset <- gets windowset
  wsIds <- getRecentWorkspaces numWorkspaces
  unless (null wsIds) $ do
    let chordMapRaw = generateChords defaultChordScheme wsIds
        chordMap = fromMaybe M.empty chordMapRaw
        wsDisplay = zipWith (curry formatWorkspace) wsIds (M.elems chordMap)

    flashText
      def {st_font = "xft:monospace:size=12"}
      (ceiling (displayTime * 100) % 100)
      (intercalate " | " wsDisplay)
  where
    formatWorkspace (ws, chord) = "[" ++ chord ++ "] " ++ ws

-- | Get list of recent workspaces from window history in proper order
getRecentWorkspaces :: Int -> X [WorkspaceId]
getRecentWorkspaces n = do
  RecentWindowsState {hist} <- XS.get
  wset <- gets windowset
  let currentWs = W.currentTag wset
      wsVisits = map (workspace . snd) $ ledger hist
      finalWs = take n $ filter (/= currentWs) $ nub wsVisits
  return finalWs
