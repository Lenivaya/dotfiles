Config
  { font = "monospace Regular 12",
    additionalFonts =
      [ "monospace Regular 12",
        "monospace Bold 12",
        "monospace Regular 10"
      ],
    bgColor = "#0b0806",
    fgColor = "#a19782",
    alpha = 255,
    -- position = TopHM 30 20 20 20 0,
    position = TopHM 30 10 10 10 0,
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
      [ -- Run Date "%d.%m.%y / %A / %H:%M" "date" 10,
        -- TODO FIXME https://discourse.mozilla.org/t/retiring-the-mozilla-location-service/128693
        -- Run ComX "openweathermap" [] "" "weather" 10000,
        -- Run UnsafeXPropertyLog "_XMONAD_LOG_1",
        Run XPropertyLog "_XMONAD_TRAYPAD",
        Run XPropertyLog "_XMONAD_LOG_1"
      ],
    sepChar = "%",
    alignSep = "}{",
    template =
      " \
      \%_XMONAD_LOG_1%\
      \}\
      \\
      \{\
      \%_XMONAD_TRAYPAD%\
      \ "
      -- \%weather%   %date% %_XMONAD_TRAYPAD%\
  }
