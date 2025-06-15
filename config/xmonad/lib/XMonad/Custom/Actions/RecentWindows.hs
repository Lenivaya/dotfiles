{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE StrictData #-}

{-|
Module      :  XMonad.Custom.Actions.RecentWindows
Description :  Functions for working with recently used windows.
Copyright   :  (c) Danylo Osipchuk, 2025
License     :  BSD3-style
Maintainer  :  danylo.osipchuk@proton.me

Provides functionality for tracking and interacting with recently used windows,
including window selection via chords or numbers.
-}
module XMonad.Custom.Actions.RecentWindows (
  -- * Configuration
  configureRecentWindows,
  maxHistorySize,

  -- * Window Selection
  withRecentWindow,
  withChordSelection,
  withNumberedSelection,

  -- * Window Display
  showRecentWindows,

  -- * Internal Functions
  getRecentWindows,

  -- * Constructors
  RecentWindowsState (..),
) where

import Control.Monad.Trans.Maybe (MaybeT (..))
import Data.Char (toLower)
import Data.List (isInfixOf, isPrefixOf, length, lines, take)
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict qualified as M
import Data.Monoid (getAll)
import Data.Ratio ((%))
import Graphics.X11.Types
import Graphics.X11.Xlib.Event
import Graphics.X11.Xlib.Extras
import XMonad
import XMonad.Actions.MostRecentlyUsed (Location (..))
import XMonad.Actions.ShowText
import XMonad.Core
import XMonad.Custom.Prompt
import XMonad.Custom.Utils.ChordKeys
import XMonad.Hooks.StatusBar.PP
import XMonad.Prelude
import XMonad.Prompt
import XMonad.Prompt.Window
import XMonad.StackSet qualified as W
import XMonad.Util.ExtensibleConf qualified as XC
import XMonad.Util.ExtensibleState qualified as XS
import XMonad.Util.History (History, erase, event, ledger, origin)
import XMonad.Util.NamedWindows (getName)
import XMonad.Util.Timer qualified as Timer
import XMonad.Util.XSelection (getSelection)

-- --< Interface >-- {{{

{- $usage

'configureRecentWindows' must be applied to your config:

> main :: IO ()
> main = xmonad . configureRecentWindows . ... $ def

Then you can use the functions in your keybindings:

> , ((modm, xK_r), showRecentWindows 3 10 0.5)  -- Show 3 recent windows
-}

-- | Maximum number of windows to keep in history
maxHistorySize :: Int
maxHistorySize = 50

-- | Configure xmonad to support recent windows tracking
configureRecentWindows :: XConfig l -> XConfig l
configureRecentWindows = XC.once f (RecentWindows ())
  where
    f cnf =
      cnf
        { logHook = logHook cnf <> logWinHist,
          handleEventHook = handleEventHook cnf <> winHistEH
        }

-- | Configuration type for Recent Windows functionality
newtype RecentWindows = RecentWindows () deriving (Semigroup)

-- | State for tracking window history
data RecentWindowsState = RecentWindowsState
  { -- | Flag to prevent recursive updates
    busy :: !Bool,
    hist :: !(History Window Location),
    -- | Cache of focused window to avoid redundant updates
    lastFocused :: !(Maybe Window)
  }
  deriving (Show, Read)

instance ExtensionClass RecentWindowsState where
  initialValue = RecentWindowsState False origin Nothing
  extensionType = PersistentExtension

-- | Represents a window with its display information
data WindowInfo = WindowInfo
  { winId :: !Window,
    winName :: !String,
    winIndex :: !(Maybe Int),
    winChord :: !(Maybe String)
  }

-- }}}

-- | Efficiently trim the history to keep only the most recent windows
trimHistory :: History Window Location -> History Window Location
trimHistory h =
  let entries = ledger h
      len = length entries
  in  if len <= maxHistorySize
        then h
        else -- Take only the most recent maxHistorySize entries and rebuild the history
          let !recentEntries = take maxHistorySize entries
          in  foldr' (\(w, loc) !hist' -> event w loc hist') origin recentEntries

-- | Log window history with check for redundant updates
logWinHist :: X ()
logWinHist = do
  wh@RecentWindowsState {busy, hist, lastFocused} <- XS.get
  unless busy $ do
    cs <- gets (W.current . windowset)
    let cws = W.workspace cs
    for_ (W.stack cws) $ \st -> do
      let currentFocus = W.focus st
      when (lastFocused /= Just currentFocus) $ do
        let location = Location {workspace = W.tag cws, screen = W.screen cs}
            updatedHist = event currentFocus location hist
            trimmedHist = trimHistory updatedHist
        XS.put $! wh {hist = trimmedHist, lastFocused = Just currentFocus}

-- | Event handler for window history events
winHistEH :: Event -> X All
winHistEH = \case
  UnmapEvent {ev_send_event = synth, ev_window = w} -> do
    e <- gets (fromMaybe 0 . M.lookup w . waitingUnmap)
    when (synth || e == 0) (collect w)
    return (All True)
  DestroyWindowEvent {ev_window = w} -> do
    collect w
    return (All True)
  _ -> return (All True)
  where
    collect w = XS.modify $ \wh@RecentWindowsState {hist} ->
      let !newHist = erase w hist in wh {hist = newHist, lastFocused = Nothing}

{-| Perform an action on a window at specified position in the MRU history.
The position is 1-based, where 1 is the most recently used window.
If the position is out of bounds, no action is performed.
-}
withRecentWindow :: Int -> (Window -> X ()) -> X ()
withRecentWindow pos action = do
  RecentWindowsState {hist} <- XS.get
  let windows = map fst (ledger hist)

  -- More efficient window lookup using a direct index check
  when (pos > 0 && pos <= length windows) $
    action (windows !! (pos - 1))

{-| Get the N most recently used windows.
Returns a list of windows in order from most recent to least recent.
-}
getRecentWindows :: Int -> X [Window]
getRecentWindows n = do
  RecentWindowsState {hist} <- XS.get
  focused <- gets (W.peek . windowset)
  let windows = map fst (ledger hist)
  -- Avoid intermediate lists and redundant filtering
  return $ take n $ filter ((/= focused) . Just) windows

-- | Data types for window prompts
data RecentWindowPrompt = RecentWindowPrompt

data ChordWindowPrompt = ChordWindowPrompt

instance XPrompt RecentWindowPrompt where
  showXPrompt RecentWindowPrompt = "Recent Window: "
  commandToComplete _ = id

instance XPrompt ChordWindowPrompt where
  showXPrompt ChordWindowPrompt = "Window Chord: "
  commandToComplete _ = id

-- | Helper functions for window management
class WindowOps a where
  getWindowInfo :: a -> X WindowInfo

instance WindowOps Window where
  getWindowInfo w = do
    name <- show <$> getName w
    return $ WindowInfo w name Nothing Nothing

-- | Common XPrompt configuration adjustments
adjustXPConfig :: Int -> XPConfig -> XPConfig
adjustXPConfig num cfg =
  cfg
    { maxComplRows = Just (fromIntegral num),
      maxComplColumns = Just 1,
      searchPredicate = \needle haystack ->
        map toLower needle `isInfixOf` map toLower haystack
    }

-- | Common window selection functionality used by both chord and numbered selection
data SelectionType = ChordType | NumberType
  deriving (Eq)

-- | Format window info based on selection type
formatWindowInfo :: SelectionType -> WindowInfo -> String
formatWindowInfo ChordType info =
  "[" ++ fromMaybe "?" (winChord info) ++ "] " ++ winName info
formatWindowInfo NumberType info =
  maybe "" show (winIndex info) ++ ": " ++ winName info

-- | Generic window selection function that works with both chord and number selection
withWindowSelection :: SelectionType -> Int -> XPConfig -> (Window -> X ()) -> X ()
withWindowSelection selType maxWindows xpconfig action = do
  let numWindows = if selType == NumberType then min 9 maxWindows else maxWindows
  windows <- getRecentWindows numWindows
  unless (null windows) $ processWindows windows
  where
    processWindows ws = do
      winInfos <- traverse getWindowInfo ws

      -- Add indices or chords based on selection type
      decoratedInfos <- case selType of
        NumberType ->
          pure $ zipWith (\i info -> info {winIndex = Just i}) [1 ..] winInfos
        ChordType -> do
          let chordScheme = customChordScheme "asdfjkl;ghqwertyuiopzxcvbnm" FrequencyBased
              chordMapRaw = generateChords chordScheme ws
              chordMap = M.fromList [(w, s) | (s, w) <- M.toList $ fromMaybe M.empty chordMapRaw]
          pure $ map (\info -> info {winChord = M.lookup (winId info) chordMap}) winInfos

      -- Generate display maps and names
      let displayMap = M.fromList [(formatWindowInfo selType i, winId i) | i <- decoratedInfos]
          displayNames = map (formatWindowInfo selType) decoratedInfos

      -- Choose the appropriate function based on selection type
      case selType of
        ChordType ->
          mkXPrompt
            ChordWindowPrompt
            (adjustXPConfig maxWindows xpconfig)
            (mkChordCompleter displayNames)
            (selectWindow displayMap)
        NumberType ->
          mkXPrompt
            RecentWindowPrompt
            (adjustXPConfig 9 xpconfig)
            (mkNumberCompleter displayNames xpconfig)
            (selectWindow displayMap)

    -- Chord matching completer
    mkChordCompleter names input = do
      return $ filter (matchChord input) names
      where
        matchChord "" = const True
        matchChord input = \name ->
          case extractChordFromBracket name of
            Just chord -> input `isPrefixOf` chord
            Nothing -> False

    -- Number matching completer
    mkNumberCompleter names xpc input = case input of
      (d : _)
        | d `elem` ['0' .. '9'] ->
            return $ filter (matchesNumber d) names
      _ -> mkComplFunFromList xpc names input
      where
        matchesNumber d str =
          let prefix = if d == '0' then "10" else [d]
          in  prefix `isPrefixOf` takeWhile (/= ':') str

    -- Common window selection function
    selectWindow displayMap str =
      whenJust (M.lookup str displayMap) action

-- | Select window using vim-style chord sequence
withChordSelection :: Int -> XPConfig -> (Window -> X ()) -> X ()
withChordSelection = withWindowSelection ChordType

-- | Select window using number keys (1-9)
withNumberedSelection :: XPConfig -> (Window -> X ()) -> X ()
withNumberedSelection = withWindowSelection NumberType 9

{-| Show N most recent windows on screen with their names.
Windows are displayed as "number:shortened_name".
-}
showRecentWindows :: Int -> Int -> Double -> X ()
showRecentWindows numWindows shortenLength displayTime = do
  windows <- getRecentWindows numWindows
  unless (null windows) $ do
    -- Use zipWithM for strictness and efficiency
    windowNames <- zipWithM formatWindow [1 ..] windows
    flashText
      def {st_font = "xft:chordmonospace:size=12"}
      (ceiling (displayTime * 100) % 100)
      (intercalate " | " windowNames)
  where
    formatWindow i w = do
      name <- show <$> getName w
      return $ show i ++ ":" ++ shorten shortenLength name
