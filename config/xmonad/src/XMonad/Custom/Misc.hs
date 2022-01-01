{-# OPTIONS_GHC -funbox-strict-fields #-}

module XMonad.Custom.Misc
  ( Applications(..)
  , applications
  ) where

data Applications = Applications
  { browser           :: !String
  , mixer             :: !String
  , notify            :: !String
  , player            :: !String
  , soundEffects      :: !String
  , term              :: !String
  , top               :: !String
  , reader            :: !String
  , editor            :: !String
  , appmenu           :: !String
  , clipboardSelector :: !String
  }
  deriving (Eq, Show)

applications :: Applications
applications = Applications
  { browser           = "$BROWSER"
  , mixer             = "pulsemixer"
  , notify            = "notify-send"
  , player            = "spotify"
  , soundEffects      = "easyeffects"
  , term              = "$TERMINAL"
  , top               = "htop"
  , reader            = "zathura"
  , editor            = "$EDITOR"
  , appmenu           = "rofi -show drun -theme main"
  , clipboardSelector =
    "rofi -theme main -modi 'clipboard:greenclip print' -show clipboard -run-command '{cmd}'"
  }
