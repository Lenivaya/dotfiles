Config
  { font = "monospace Regular 12"
  , additionalFonts =
      [
       "monospace Bold 12"
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
      [
      Run
          DynNetwork
          ["-t", "<fn=2></fn> <rx> / <fn=2></fn> <tx> kbps"]
          10
      , Run Date "%d.%m.%y / %A / %H:%M" "date" 10
      , Run MultiCpu ["-t", "<fn=2></fn> <total>%"]  10
      , Run MultiCpu ["-t", "<fn=2></fn> <total>%"]  10
      , Run Memory ["-t", "<fn=2></fn> <usedratio>%"] 10
      , Run ComX "player" [] "" "player" 10
      , Run ComX "caffeinatestatus" [] "" "caffeine" 10
      , Run ComX "warpstatus" [] "" "warp" 10
      ]
  , sepChar = "%"
  , alignSep = "}{"
  , template =
      " \
      \%memory%  |  %multicpu%  |  %dynnetwork%\
      \}\
      \%player%\
      \{\
      \%warp% %caffeine%   |  %date%\
      \ "
  }
