{-# OPTIONS_GHC -funbox-strict-fields #-}

module XMonad.Custom.Misc (
  Applications (..),
  applications,
) where

data Applications = Applications
  { browser :: !String,
    mixer :: !String,
    notify :: !String,
    player :: !String,
    soundEffects :: !String,
    term :: !String,
    termSmallFont :: !String,
    top :: !String,
    reader :: !String,
    editor :: !String,
    appmenu :: !String,
    -- , clipboardSelector :: !String
    virtualMachinesManger :: !String,
    screenZoomer :: !String
  }
  deriving (Eq, Show)

applications :: Applications
applications =
  Applications
    { browser = "$BROWSER",
      mixer = "pulsemixer",
      notify = "notify-send",
      player = "spotify",
      soundEffects = "easyeffects",
      term = "$TERM",
      termSmallFont = "$TERM -o font.size=8",
      -- top = "htop",
      top = "btop",
      reader = "zathura",
      editor = "$EDITOR",
      appmenu = "rofi_drun",
      -- , clipboardSelector =
      --     "rofi -modi 'clipboard:greenclip print' -show clipboard -run-command '{cmd}'"
      virtualMachinesManger = "virt-manager",
      screenZoomer = "boomer"
    }
