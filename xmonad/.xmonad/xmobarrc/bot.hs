Config { font = "xft:tewi:style=Regular:pixelsize=11,Biwidth:pixelsize=12"
       , additionalFonts = [ "xft:Siji:style=Regular"
                           , "xft:tewi:style=Bold:pixelsize=11"
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
       , borderWidth = 1 , iconRoot = "." , commands = [ Run StdinReader , Run Battery [ "-t", "<fn=1><acstatus></fn><left>%"
                                  , "--"
                                  , "-i", "\57914", "-O", "\57913" , "-o", "\57911"
                                  ] 10 , Run Cpu [ "-t", "<fn=1>\57381</fn><total>%" ] 10
                    , Run CoreTemp [ "-t", "<core0>°C / <core1>°C" ] 10
                    , Run Memory [ "-t", "<fn=1>\57384</fn> <usedratio>%" ] 10
                    , Run ThermalZone 0 ["-t","<fn=1>\57371</fn> <temp>°C"] 10
                    , Run ThermalZone 1 ["-t","<temp>°C"] 10
                    , Run Wireless "wlp3s0" [ "-t", "<fn=1>\57775</fn><essid> @ <quality>%" ] 10
                    , Run DynNetwork [ "-t", "<fn=1>\57660</fn><rx> / <fn=1>\57659</fn><tx> kbps" ] 10
                    , Run Kbd [ ("us", "<fn=1>\57967</fn>English")
                              , ("ru", "<fn=1>\57967</fn>Russian")
                              , ("ua","<fn=1>\57967</fn>Ukrainian")]
                    , Run Com "/home/free/.xmonad/scripts/xmobar/fcitx.sh" [] "fcitx" 3
                    , Run Locks
                    , Run MPD [ "-t", "<fn=1><statei></fn> <artist> - <title>   /"
                              , "--"
                              , "-P", "\57498", "-Z", "\57499", "-S", "\57497"
                              ] 10
                    , Run Volume "default" "Master" [ "-t", "<fn=1><status></fn><volume>%"
                                                    , "--"
                                                    , "--on"   , "\57427"
                                                    , "--off"  , "\57426"
                                                    , "--onc"  , "#a19782"
                                                    , "--offc" , "#a19782"
                                                    ] 10
                    , Run Mpris2 "spotify" ["-t", "<artist> - <title>"] 10
                    ]
       , sepChar = "%"
       , alignSep = "}{"
       , template = " \
                    \%battery%   %wlp3s0wi%   %dynnetwork%   %StdinReader%\
                    \}\
                    \\
                    \{\
                    \%mpris2%  /  %kbd%   %default:Master%\
                    \ "
       }

-- vim:filetype=haskell
