Config
  {
    font = "monospace Regular 12",
    additionalFonts =
      [
        -- "xft:Siji:style=Regular:size=12",
        -- "xft:monospace:style=Regular:size=12",
        -- "xft:Weather Icons:style=Regular:size=10:hinting=true",
        -- "xft:FontAwesome:style=Regular:size=10:hinting=true"
        "Siji Regular 12",
        "monospace Regular 12",
        "Weather Icons Regular 10",
        "FontAwesome Regular 10"
      ],
    bgColor = "#0b0806",
    fgColor = "#a19782",
    alpha = 255,
    position = TopSize C 100 24,
    -- textOffset = 20,
    -- textOffsets = [20, 20],
    iconOffset = -1,
    lowerOnStart = True,
    hideOnStart = False,
    allDesktops = True,
    overrideRedirect = False,
    pickBroadest = False,
    persistent = False,
    border = FullBM 0,
    borderColor = "#2f2b2a",
    borderWidth = 1,
    iconRoot = ".",
    commands =
      [
        Run UnsafeXPropertyLog "_XMONAD_LOG_1",
        -- Run UnsafeXMonadLog,
        Run Date "%d.%m.%y / %A / %H:%M" "date" 10,
        -- Run Weather "UUWW" ["-t", "<fn=1>\57550</fn><tempC>°C / <rh>% / <pressure> Pa"] 10000,
        Run ComX "openweathermap" [] "" "weather" 10000
      ],
    sepChar = "%",
    alignSep = "}{",
    template =
      " \
      \%_XMONAD_LOG_1%\
      \}\
      \\
      \{\
      \%weather%   %date%\
      \ "
      -- " \
      -- \%UnsafeStdinReader%\
      -- \}\
      -- \\
      -- \{\
      -- \%weather%   %date%\
      -- \ "
  }
