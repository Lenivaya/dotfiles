{-# OPTIONS_GHC -funbox-strict-fields #-}

module XMonad.Custom.Misc
  ( Applications(..)
  , applications
  )
where

data Applications = Applications
    { browser   ::  !String
    , mixer     ::  !String
    , notify    ::  !String
    , player    ::  !String
    , term      ::  !String
    , top       ::  !String
    , reader    ::  !String
    , editor    ::  !String
    , appmenu   ::  !String
    } deriving (Eq, Show)

applications :: Applications
applications = Applications { browser = "$BROWSER"              --  "google-chrome-stable --force-dark-mode"
                            , mixer   = "pulsemixer"
                            , notify  = "notify-send"
                            , player  = "spotify"
                            , term    = "$TERMINAL"
                            , top     = "htop"
                            , reader  = "zathura"
                            , editor  = "$EDITOR"
                            , appmenu = "rofi -show drun"
                            }
