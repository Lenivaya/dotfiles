Config
  { font = "monospace Regular 12",
    additionalFonts =
      [ "monospace Bold 14"
      ],
    bgColor = "#0b0806",
    fgColor = "#a19782",
    alpha = 255,
    -- position = BottomHM 30 20 20 0 20,
    position = BottomHM 30 10 10 0 10,
    iconOffset = -1,
    lowerOnStart = True,
    hideOnStart = False,
    allDesktops = True,
    overrideRedirect = True,
    pickBroadest = False,
    persistent = True,
    border = FullBM 0,
    borderColor = "#2f2b2a",
    borderWidth = 1,
    iconRoot = ".",
    commands =
      [ Run
          DynNetwork
          [ "-t",
            "<fn=1>\61813</fn> <rx> / <fn=1>\61813</fn> <tx> kbps"
          ]
          10,
        Run Date "%d.%m.%y / %A / %H:%M" "date" 10,
        Run DateZone "<fc=#8f8b8a>(UTC: %H:%M)</fc>" "" "UTC" "utcdate" 10,
        Run DateZone "<fc=#8f8b8a>(AS: %H:%M)</fc>" "" "Asia/Tokyo" "asiadate" 10,
        Run DateZone "<fc=#8f8b8a>(US: %H:%M)</fc>" "" "America/New_York" "usdate" 10,
        Run MultiCpu ["-t", "<fn=1>\62652</fn> <total>%"] 20,
        Run Memory ["-t", "<fn=1>\61381</fn>  <usedratio>%"] 20,
        Run ComX "player" [] "" "player" 20,
        Run ComX "caffeinatestatus" [] "" "caffeine" 20
        -- , Run ComX "warpstatus" [] "" "warp" 10
      ],
    sepChar = "%",
    alignSep = "}{",
    template =
      " \
      \%memory%  |  %multicpu%  |  %dynnetwork%\
      \}\
      \%player%\
      \{\
      \%caffeine%   |  %date% %utcdate% %asiadate% %usdate%\
      \ "
  }
