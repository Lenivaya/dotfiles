{-# LANGUAGE ImportQualifiedPost #-}

module XMonad.Custom.Actions.DoActions (
  Action (..),
  doAction,
  allActions,
  findActionByName,
  spawnBrowserWithUrls,
  spawnBrowserWithUrl,
  spawnTerminalWith,
) where

import Control.Monad (unless)
import Data.Char (toLower)
import Data.Function (on)
import Data.List (sortBy)
import XMonad
import XMonad.Actions.DynamicProjects
import XMonad.Actions.Minimize
import XMonad.Hooks.ManageDocks
import XMonad.Layout.Hidden
import XMonad.Layout.MultiToggle
import XMonad.Layout.MultiToggle.Instances
import XMonad.Util.NamedScratchpad
import XMonad.Util.Run

import XMonad.Custom.Actions.ApplicationChooser
import XMonad.Custom.Actions.Keyboard
import XMonad.Custom.Actions.Screen.Screencast
import XMonad.Custom.Actions.Screen.Screenshot
import XMonad.Custom.Hooks.Layout qualified as L
import XMonad.Custom.Misc qualified as C
import XMonad.Custom.Prompt
import XMonad.Custom.Scratchpads

{-| Simple data type for actions
  The Eq/Ord instances are based on the action name for easy sorting and comparison.
-}
data Action = Action
  { actionName :: String, -- Human-readable name
    actionFunction :: X () -- The XMonad action to execute
  }

instance Eq Action where
  a == b = actionName a == actionName b

instance Ord Action where
  compare = compare `on` actionName

-- | Execute an action
doAction :: Action -> X ()
doAction = actionFunction

{-| Find an action by its name (O(n)).
  For a large number of actions or frequent lookups, consider using a Map.
-}
findActionByName :: String -> Maybe Action
findActionByName name = lookup name [(actionName a, a) | a <- allActions]

-- | Spawn browser with multiple URLs
spawnBrowserWithUrls :: [String] -> X ()
spawnBrowserWithUrls [] = return ()
spawnBrowserWithUrls (firstUrl : otherUrls) = do
  spawn $ C.browser C.applications ++ " --new-window " ++ firstUrl
  unless (null otherUrls) $
    spawn $
      C.browser C.applications ++ " " ++ unwords otherUrls

-- | Spawn browser with a single URL
spawnBrowserWithUrl :: String -> X ()
spawnBrowserWithUrl url = spawn $ C.browser C.applications ++ " --new-window " ++ url

-- | Spawn terminal with command
spawnTerminalWith :: String -> X ()
spawnTerminalWith cmd = spawn $ C.term C.applications ++ " " ++ cmd

{-| List of all available actions, sorted case-insensitively by name for prompt UIs.
  If you want a different order, move the sort to the UI layer.
-}
allActions :: [Action]
allActions =
  sortBy
    (compare `on` (map toLower . actionName))
    [ Action "Terminal" $
        spawn $
          C.term C.applications,
      Action "Terminal with Tmux" $
        spawn $
          C.term C.applications ++ " -e tmux",
      Action "Browser" $
        selectBrowserByNameAndDo promptTheme spawn,
      Action "Editor" $
        selectEditorByNameAndDo promptTheme spawn,
      Action "File Manager" $
        spawn $
          C.term C.applications ++ " -e yazi",
      Action "Console Scratchpad" $
        namedScratchpadAction scratchpads "console",
      Action "Music Scratchpad" $
        namedScratchpadAction scratchpads "music",
      Action "System Monitor" $
        namedScratchpadAction scratchpads "top",
      Action "Volume Control" $
        namedScratchpadAction scratchpads "volume",
      Action "Sound Effects" $
        namedScratchpadAction scratchpads "soundEffects",
      Action "Notes" $
        namedScratchpadAction scratchpads "notes",
      Action "Notes (Neovim)" $
        namedScratchpadAction scratchpads "notes-nvim",
      Action "GitButler" $
        spawn "gitbutler-tauri",
      Action "AI Assistants" $ do
        spawnBrowserWithUrl "https://www.perplexity.ai"
        spawnBrowserWithUrls
          [ "https://chat.openai.com",
            "https://claude.ai",
            "https://gemini.google.com/app",
            "https://chat.deepseek.com"
          ],
      Action "AI Research" $ do
        spawnBrowserWithUrls
          [
            "https://grok.com/",
            "https://scira.ai/",
            "https://notebooklm.google.com/",
            "https://www.perplexity.ai/"
          ],
      Action "GitHub" $ do
        spawnBrowserWithUrl "https://github.com"
        spawnBrowserWithUrls ["https://github.com/notifications", "https://github.com/pulls"],
      Action "System Monitors" $ do
        spawnTerminalWith "-e btop"
        spawnTerminalWith "-e htop",
      Action "Mail" $ do
        spawnBrowserWithUrl "https://mail.google.com"
        spawnBrowserWithUrl "https://mail.proton.me",
      Action "Social Media" $
        spawnBrowserWithUrl "https://x.com",
      Action "Disk Space Analyzer" $ do
        spawnTerminalWith "--hold -e duf"
        spawnTerminalWith "--hold , bleachbit"
        spawnTerminalWith "-e dua i ~/",
      Action "Weather" $ do
        spawnTerminalWith "--hold -e curl wttr.in"
        spawnBrowserWithUrl "https://merrysky.net/",
      Action "Development Environment" $ do
        spawn "cursor"
        spawnTerminalWith "-e tmux"
        spawnBrowserWithUrl "https://devdocs.io",
      Action "Network Monitors" $ do
        spawnTerminalWith "--hold -e , speedtest-rs"
        spawnTerminalWith "-e , bmon",
      Action "System Logs" $
        spawnTerminalWith "--hold -e journalctl -f",
      Action "API Client" $
        spawn "postman",
      Action "Language Learning" $
        spawnBrowserWithUrls
          [ "https://duolingo.com",
            "https://translate.google.com",
            "https://forvo.com"
          ],
      Action "Translation Tools" $
        spawnBrowserWithUrls
          [ "https://translate.google.com",
            "https://deepl.com",
            "https://reverso.net"
          ],
      Action "Docker Tools" $ do
        spawnTerminalWith "-e lazydocker"
        spawnTerminalWith "-e oxker",
      Action "Cryptocurrency Tools" $ do
        spawnTerminalWith "-e , cointop"
        spawnBrowserWithUrls ["https://x.com", "https://dropstab.com"],
      Action "Screen Recorder" $
        spawn "obs",
      Action "Video Streaming" $
        spawnBrowserWithUrls
          [ "https://youtube.com",
            "https://www.youtube.com/playlist?list=WL",
            "https://www.youtube.com/feed/history"
          ],
      Action "Upwork" $ do
        spawnBrowserWithUrls
          [ "https://www.upwork.com/",
            "https://www.upwork.com/nx/find-work/best-matches",
            "https://www.upwork.com/nx/plans/connects/history/"
          ]
        spawn "upwork",
      Action "Entertainment Mode" $ do
        spawnBrowserWithUrls
          [ "https://youtube.com"
          , "https://music.youtube.com"
          , "https://x.com"
          ],
      Action "Dotfiles" $ do
        spawnTerminalWith "--hold -e $DOTFILES"
        spawnTerminalWith "-e nvim $DOTFILES",
      -- Window management actions
      Action "Toggle Fullscreen" $
        sendMessage $
          Toggle NBFULL,
      Action "Toggle Gaps" $
        L.toggleGaps,
      Action "Toggle Statusbar" $
        L.toggleStatusBar,
      Action "Toggle Zen Mode" $
        L.toggleZen,
      Action "Minimize Window" $
        withFocused minimizeWindow,
      Action "Restore Last Minimized" $
        withLastMinimized maximizeWindowAndFocus,
      Action "Hide Window" $
        withFocused hideWindow,
      Action "Unhide Last" $
        popNewestHiddenWindow,
      -- Screenshot and screencast actions
      Action "Screenshot Full" $
        screenshot Fullscreen,
      Action "Screenshot Selection" $
        screenshot Select,
      Action "Screenshot to Clipboard" $
        screenshot SelectCopyToClipboard,
      Action "Screencast mp4" $
        screencast RecordToMP4,
      Action "Screencast gif" $
        screencast RecordToGIF,
      Action "System Health Check" $ do
        spawnTerminalWith "-e btop"
        spawnTerminalWith "-e htop"
        spawnTerminalWith "--hold -e journalctl -f",
      Action "Template" $ do
        spawnTerminalWith "-e tmux"
        spawn (C.browser C.applications)
    ]
