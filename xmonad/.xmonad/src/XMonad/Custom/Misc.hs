{-# OPTIONS_GHC -funbox-strict-fields #-}

module XMonad.Custom.Misc
    ( Applications (..)
    , applications
    ) where

data Applications = Applications
    { browser :: !String
    , mixer   :: !String
    , notify  :: !String
    , player  :: !String
    , term    :: !String
    , top     :: !String
    } deriving (Eq, Show)

applications :: Applications
applications = Applications
    { browser = "qutebrowser"
    , mixer   = "pulsemixer"
    , notify  = "notify-send"
    , player  = "ncmpcpp"
    , term    = "st"
    , top     = "htop"
    }
