{-# OPTIONS_GHC -funbox-strict-fields #-}

module XMonad.Custom.Misc
    ( Applications (..)
    , applications
    ) where

data Applications = Applications
    { browser  ::  !String
    , mixer    ::  !String
    , notify   ::  !String
    , player   ::  !String
    , term     ::  !String
    , termfont ::  !String
    , top      ::  !String
    , reader   ::  !String
    , emacs    ::  !String
    } deriving (Eq, Show)

applications :: Applications
applications = Applications
    { browser  = "qutebrowser"
    , mixer    = "pulsemixer"
    , notify   = "notify-send"
    , player   = "ncmpcpp"
    , term     = "st"
    , termfont = "st -f Iosevka-Term:pixelsize=16"
    , top      = "htop"
    , reader   = "zathura"
    , emacs    = "st -f Iosevka-Term:pixelsize=16 -e emacsclient -t -c"
    }
