Config { font = "xft:monospace:style=Regular:pixelsize=11,Biwidth:pixelsize=12"
       , additionalFonts = [ "xft:Siji:style=Regular"
                           , "xft:monospace:style=Bold:pixelsize=11"
                           ]
       , bgColor = "#0b0806"
       , fgColor = "#a19782"
       , alpha = 255
       , position = BottomSize C 100 24
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
       , commands = [  Run StdinReader
           ,Run Battery [ "-t", "<fn=1><acstatus></fn> <left>%"
                                  , "--"
                                  , "-i", "\57914", "-O", "\57913" , "-o", "\57911"
                                  ,"-a", "notify-send -u critical 'Battery running out!!! Only 10% left'"
                                  ,"-A", "10"
                                  ] 10
                    , Run Wireless "wlp3s0" [ "-t", "<fn=1>\57775</fn> <essid> @ <quality>%" ] 10
                    , Run DynNetwork [ "-t", "<fn=1>\57660</fn> <rx> / <fn=1>\57659</fn> <tx> kbps" ] 10
                    , Run Kbd [ ("us", "<fn=1>\57967</fn> English")
                              , ("ru", "<fn=1>\57967</fn> Russian")
                              , ("ua","<fn=1>\57967</fn> Ukrainian")]
                    , Run Volume "default" "Master" [ "-t", "<fn=1><status></fn> <volume>%"
                                                    , "--"
                                                    , "--on"   , "\57427"
                                                    , "--off"  , "\57426"
                                                    , "--onc"  , "#a19782"
                                                    , "--offc" , "#a19782"
                                                    ] 1
                    , Run Mpris2 "spotify" ["-t", "<artist> - <title>"] 10
                    ]
       , sepChar = "%"
       , alignSep = "}{"
       , template = " \
                    \%battery%   %wlp3s0wi%   %dynnetwork%  %StdinReader%\
                    \}\
                    \\
                    \{\
                    \%mpris2%  /  %kbd%   %default:Master%\
                    \ "
       }
