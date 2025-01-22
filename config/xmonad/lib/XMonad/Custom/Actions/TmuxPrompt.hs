module XMonad.Custom.Actions.TmuxPrompt (tmuxPrompt) where

import Data.List (lines)
import XMonad
import XMonad.Prompt
import XMonad.Util.Run (runInTerm, runProcessWithInput)

data Tmux = Tmux

instance XPrompt Tmux where
  showXPrompt Tmux = "Tmux session: "

tmuxPrompt :: XPConfig -> X ()
tmuxPrompt c = do
  sessions <- getSessions
  mkXPrompt Tmux c (mkComplFunFromList' c sessions) startTmux

getSessions :: MonadIO m => m [String]
getSessions = io $ do
  output <- runProcessWithInput "tmux" ["list-sessions", "-F", "#S"] ""
  return $ lines output

startTmux :: String -> X ()
startTmux s =
  runInTerm "" $
    "tmux attach-session -t \""
      ++ s
      ++ "\" || tmux new-session -A -s \""
      ++ s
      ++ "\""
