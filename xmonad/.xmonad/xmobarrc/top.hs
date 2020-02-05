Config { font = "xft:tewi:style=Regular:pixelsize=11,Biwidth:pixelsize=12"
       , additionalFonts = [ "xft:Siji:style=Regular"
                           , "xft:tewi:style=Bold:pixelsize=11"
                           ]
       , bgColor = "#0b0806"
       , fgColor = "#a19782"
       , alpha = 255
       , position = TopSize C 100 24
       , textOffset = 15
       , textOffsets = [15, 15]
       , iconOffset = -1
       , lowerOnStart = True
       , hideOnStart = False
       , allDesktops = True
       , overrideRedirect = False
       , pickBroadest = False
       , persistent = False
       , border = FullBM 0
       , borderColor = "#2f2b2a"
       , borderWidth = 1
       , iconRoot = "."
       , commands = [ Run StdinReader
                    , Run Date "<fn=1>\57893</fn>%d.%m.%y / %A / %H:%M" "date" 10
                    , Run Weather "UUWW" [ "-t", "<fn=1>\57550</fn> <tempC>°C / <rh>% / <pressure> Pa" ] 10000
                    ]
       , sepChar = "%"
       , alignSep = "}{"
       , template = " \
                    \%StdinReader%\
                    \}\
                    \\
                    \{\
                    \%date%\
                    \ "
       }

-- vim: filetype=haskell
