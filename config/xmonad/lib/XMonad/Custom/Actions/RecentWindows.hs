{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost #-}
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
    hist :: !(History Window Location)
  }
  deriving (Show, Read)

instance ExtensionClass RecentWindowsState where
  initialValue = RecentWindowsState False origin
  extensionType = PersistentExtension

-- | Represents a window with its display information
data WindowInfo = WindowInfo
  { winId :: Window,
    winName :: String,
    winIndex :: Maybe Int,
    winChord :: Maybe String
  }

-- }}}

-- | Trim the history to keep only the most recent windows
trimHistory :: History Window Location -> History Window Location
trimHistory h =
  let entries = ledger h
      len = length entries
  in  if len <= maxHistorySize
        then h
        else -- Rebuild history with only the most recent maxHistorySize entries
          foldr (\(w, loc) hist -> event w loc hist) origin (take maxHistorySize entries)

logWinHist :: X ()
logWinHist = do
  wh@RecentWindowsState {busy, hist} <- XS.get
  unless busy $ do
    cs <- gets (W.current . windowset)
    let cws = W.workspace cs
    for_ (W.stack cws) $ \st -> do
      let location = Location {workspace = W.tag cws, screen = W.screen cs}
          updatedHist = event (W.focus st) location hist
          trimmedHist = trimHistory updatedHist
      XS.put wh {hist = trimmedHist}

winHistEH :: Event -> X All
winHistEH ev =
  All True <$ case ev of
    UnmapEvent {ev_send_event = synth, ev_window = w} -> do
      e <- gets (fromMaybe 0 . M.lookup w . waitingUnmap)
      when (synth || e == 0) (collect w)
    DestroyWindowEvent {ev_window = w} -> collect w
    _ -> pure ()
  where
    collect w = XS.modify $ \wh@RecentWindowsState {hist} -> wh {hist = erase w hist}

-- }}}

{-| Perform an action on a window at specified position in the MRU history.
The position is 1-based, where 1 is the most recently used window.
If the position is out of bounds, no action is performed.

Example usage:
> withRecentWindow 2 windows W.swapUp  -- Swap the second most recent window up
> withRecentWindow 1 windows W.sink    -- Sink the most recent window
> withRecentWindow 3 (windows . W.focusWindow) -- Focus the third most recent window
-}
withRecentWindow :: Int -> (Window -> X ()) -> X ()
withRecentWindow pos action = do
  RecentWindowsState {hist} <- XS.get
  case ledger hist of
    [] -> return () -- No windows in history
    ws -> do
      let winList = map fst ws -- Extract just the Window from each tuple
      -- Check if position is valid (convert from 1-based to 0-based)
      let zeroBasedPos = pos - 1
      case listToMaybe $ drop zeroBasedPos winList of
        Just win | pos > 0 -> action win
        _ -> return ()

{-| Get the N most recently used windows.
Returns a list of windows in order from most recent to least recent.
If N is larger than the number of available windows, returns all available windows.

Example usage:
> getRecentWindows 2  -- Get the two most recent windows
> getRecentWindows 5  -- Get the five most recent windows
-}
getRecentWindows :: Int -> X [Window]
getRecentWindows n = do
  RecentWindowsState {hist} <- XS.get
  focused <- gets (W.peek . windowset)
  -- Get all entries from the history ledger
  let entries = ledger hist
      -- Extract windows with their usage timestamps, most recent first
      windows = map fst entries
      -- Filter out currently focused window and take n elements
      recentWindows = take n $ filter (\w -> Just w /= focused) windows
  return recentWindows

{-| Show a prompt with numbered recent windows and allow selection by number or fuzzy search.
The function provides two ways to select a window:
1. Type a number to quickly select window by its position (1-9)
2. Type text to fuzzy search window names

The prompt shows windows in format "number: window_name".
Current (focused) window is excluded from the list.

Example usage:
> withNumberedWindows promptTheme (windows . W.focusWindow)  -- Focus selected window with custom prompt theme
> withNumberedWindows def (windows . W.swapUp)              -- Swap selected window up using default theme
> withNumberedWindows myXPConfig (windows . W.sink)         -- Sink selected window using custom XPConfig

Parameters:
  * xpconfig - XPrompt configuration determining appearance and behavior of the selection prompt
  * action   - Function to apply to the selected window
-}

-- | Data types for window prompts
data RecentWindowPrompt = RecentWindowPrompt

data ChordWindowPrompt = ChordWindowPrompt

instance XPrompt RecentWindowPrompt where
  showXPrompt RecentWindowPrompt = "Recent Window: "
  commandToComplete _ = id

instance XPrompt ChordWindowPrompt where
  showXPrompt ChordWindowPrompt = "Window Chord: "
  commandToComplete _ = id

{-| Show recent windows with chord-based selection.
Uses vim-like chord sequences for quick selection, falling back to fuzzy search
if the input doesn't match any chord.
-}

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

-- | Select window using vim-style chord sequence
withChordSelection :: Int -> XPConfig -> (Window -> X ()) -> X ()
withChordSelection numWindows xpconfig action = do
  -- Get windows in order (most recent first)
  windows <- getRecentWindows numWindows
  unless (null windows) $ processWindowSelection windows
  where
    processWindowSelection :: [Window] -> X ()
    processWindowSelection ws = do
      -- Windows are already in MRU order from getRecentWindows
      winInfos <- mapM getWindowInfo ws
      let chordScheme = customChordScheme "asdfjkl;ghqwertyuiopzxcvbnm" FrequencyBased
          chordMapRaw = generateChords chordScheme ws
          chordMap = M.fromList [(w, s) | (s, w) <- M.toList $ fromMaybe M.empty chordMapRaw]
          decoratedInfos = map (addChordInfo chordMap) winInfos

      showWindowPrompt decoratedInfos

    addChordInfo :: M.Map Window String -> WindowInfo -> WindowInfo
    addChordInfo chordMap info =
      info {winChord = M.lookup (winId info) chordMap}

    showWindowPrompt infos = do
      -- Use lists to preserve the recency ordering instead of Map's keys
      let displayMap = M.fromList [(formatWindowEntry i, winId i) | i <- infos]
          displayNames = map formatWindowEntry infos

      mkXPrompt
        ChordWindowPrompt
        (adjustXPConfig numWindows xpconfig)
        (mkChordCompleter displayNames)
        (selectWindow displayMap)

    formatWindowEntry info =
      "[" ++ fromMaybe "?" (winChord info) ++ "] " ++ winName info

    mkChordCompleter names input = do
      return $ filter (matchChord input) names
      where
        matchChord "" = const True
        matchChord input = \name ->
          case extractChordFromBracket name of
            Just chord -> input `isPrefixOf` chord
            Nothing -> False

    selectWindow displayMap str =
      whenJust (M.lookup str displayMap) action

{-| Original numbered selection implementation
| Select window using number keys (1-9)
-}
withNumberedSelection :: XPConfig -> (Window -> X ()) -> X ()
withNumberedSelection xpconfig action = do
  -- Get windows in order (most recent first), limited to 9 for number keys
  windows <- getRecentWindows 9
  unless (null windows) $ processNumberedWindows windows
  where
    processNumberedWindows ws = do
      -- Preserve order by using traverse instead of mapM
      winInfos <- traverse getWindowInfo ws
      -- Use strict zipWith to maintain ordering
      let numberedInfos = zipWith' addIndex [1 ..] winInfos
      showNumberedPrompt numberedInfos
      where
        zipWith' f (x : xs) (y : ys) = f x y : zipWith' f xs ys
        zipWith' _ _ _ = []

    addIndex i info = info {winIndex = Just i}

    showNumberedPrompt infos = do
      -- Use lists to preserve the recency ordering instead of Map's keys
      let displayMap = M.fromList [(formatNumberedEntry i, winId i) | i <- infos]
          displayNames = map formatNumberedEntry infos

      mkXPrompt
        RecentWindowPrompt
        (adjustXPConfig 9 xpconfig)
        (mkNumberCompleter displayNames)
        (selectWindow displayMap)

    formatNumberedEntry info =
      maybe "" show (winIndex info) ++ ": " ++ winName info

    mkNumberCompleter names input = case input of
      (d : _)
        | d `elem` ['0' .. '9'] ->
            return $ filter (matchesNumber d) names
      _ -> mkComplFunFromList xpconfig names input
      where
        matchesNumber d str =
          let prefix = if d == '0' then "10" else [d]
          in  prefix `isPrefixOf` takeWhile (/= ':') str

    selectWindow displayMap =
      flip whenJust action . flip M.lookup displayMap

{-| Show N most recent windows on screen with their names.
Windows are displayed as "number:shortened_name".
Names are shortened to specified length with "..." if needed.

Example usage:

showRecentWindows
  :: Int
  -- ^ Number of windows to show
  -> Int
  -- ^ Length to shorten window names to
  -> Double
  -- ^ Time in seconds to show the text
  -> X ()
-}
showRecentWindows :: Int -> Int -> Double -> X ()
showRecentWindows numWindows shortenLength displayTime = do
  -- Reverse the windows list to match the display order we want (most recent first)
  windows <- reverse <$> getRecentWindows numWindows
  unless (null windows) $ do
    windowNames <- forM (zip [1 ..] windows) $ \(i, w) -> do
      name <- show <$> getName w
      return $ show i ++ ":" ++ shorten shortenLength name

    flashText
      def {st_font = "xft:chordmonospace:size=12"}
      (ceiling (displayTime * 100) % 100)
      (intercalate " | " windowNames)
