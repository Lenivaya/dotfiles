Config
  { font = "monospace Regular 12"
  , additionalFonts =
      [ "xft:Siji Regular"
      , "monospace Bold 12"
      , "Font Awesome 6 Free Regular 10"
      ]
  , bgColor = "#0b0806"
  , fgColor = "#a19782"
  , alpha = 255
  , -- position = BottomHM 30 10 10 0 10,
    position = BottomHM 30 20 20 0 20
  , iconOffset = -1
  , lowerOnStart = True
  , hideOnStart = False
  , allDesktops = True
  , overrideRedirect = True
  , pickBroadest = False
  , persistent = False
  , border = FullBM 0
  , borderColor = "#2f2b2a"
  , borderWidth = 1
  , iconRoot = "."
  , commands =
      [ -- Run
        --   Battery
        --   [ "-t"
        --   , "<fn=3><acstatus></fn> <left>%"
        --   , "--"
        --   , "-i"
        --   , "\62016"
        --   , "-O"
        --   , "\62020  \61671"
        --   , "-o"
        --   , "\62018"
        --   , -- "-a",
        --     -- "notify-send -u critical 'Battery running out!!!'",
        --     "-A"
        --   , "5"
        --   ]
        --   10
        -- ,
        -- Run Wireless "wlp4s0" ["-t", "<fn=1>\57775</fn> <essid> @ <quality>%"] 10
        Run Wireless "wlan0" ["-t", "<fn=3></fn> <ssid> @ <quality>%"] 10
      , Run
          DynNetwork
          -- ["-t", "<fn=1>\57660</fn> <rx> / <fn=1>\57659</fn> <tx> kbps"]
          ["-t", "<fn=3></fn> <rx> / <fn=3></fn> <tx> kbps"]
          10
      , Run
          Kbd
          [ ("us", "<fn=3></fn> English")
          , ("ru", "<fn=3></fn> Русский")
          , ("ua", "<fn=3></fn> Українська")
          ]
      , Run
          Alsa
          "default"
          "Master"
          [ "-t"
          , "<fn=3><status></fn> <volume>%"
          , "--"
          , "--on"
          , ""
          , "--off"
          , ""
          , "--onc"
          , "#a19782"
          , "--offc"
          , "#a19782"
          -- , "--on"
          -- , "\57427"
          -- , "--off"
          -- , "\57426"
          -- , "--onc"
          -- , "#a19782"
          -- , "--offc"
          -- , "#a19782"
          ]
      -- , Run MultiCpu ["-t", "<fn=3></fn> <total>%"]  10
      -- , Run Memory ["-t", "<fn=3></fn> <usedratio>%"] 10
      , Run ComX "player" [] "" "player" 10
      , Run ComX "caffeinatestatus" [] "" "caffeine" 10
      , Run ComX "warpstatus" [] "" "warp" 10
      ]
  , sepChar = "%"
  , alignSep = "}{"
  , template =
      " \
      \%wlan0wi%  -  %dynnetwork%\
      \}\
      \%player%\
      \{\
      \%warp% %caffeine%   %kbd%   %alsa:default:Master%\
      \ "
  }
