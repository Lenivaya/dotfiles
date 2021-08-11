Config
  { font = "xft:monospace:style=Regular:size=10,Biwidth:pixelsize=11",
    additionalFonts =
      [ "xft:Siji:style=Regular",
        "xft:monospace:style=Bold:size=10",
        "xft:FontAwesome:style=Regular:size=8:hinting=true"
      ],
    bgColor = "#0b0806",
    fgColor = "#a19782",
    alpha = 255,
    position = BottomSize C 100 24,
    textOffset = 20,
    textOffsets = [20, 20],
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
        [ Run StdinReader,
          Run
          Battery
          [ "-t",
            "<fn=3><acstatus></fn> <left>%",
            "--",
            "-i",
            "",
            "-O",
            "  ",
            "-o",
            "",
            "-a",
            "notify-send -u critical 'Battery running out!!!'",
            "-A",
            "5"
          ]
          10,
        Run Wireless "wlp4s0" ["-t", "<fn=1>\57775</fn><essid> @ <quality>%"] 10,
        Run DynNetwork ["-t", "<fn=1>\57660</fn><rx> / <fn=1>\57659</fn><tx> kbps"] 10,
        Run
          Kbd
          [ ("us", "<fn=1>\57967</fn>English"),
            ("ru", "<fn=1>\57967</fn>Русский"),
            ("ua", "<fn=1>\57967</fn>Українська")
          ],
        Run
          Volume
          "default"
          "Master"
          [ "-t",
            "<fn=1><status></fn><volume>%",
            "--",
            "--on",
            "\57427",
            "--off",
            "\57426",
            "--onc",
            "#a19782",
            "--offc",
            "#a19782"
          ]
          10,
        Run ComX "player" [] "" "player" 10
      ],
    sepChar = "%",
    alignSep = "}{",
    template =
      " \
      \%battery%   %wlp4s0wi%   %dynnetwork%   %StdinReader%\
      \}\
      \%player%\
      \{\
      \%kbd%   %default:Master%\
      \ "
  }
