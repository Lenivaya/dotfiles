module XMonad.Custom.Theme
  ( font
  , black1
  , black2
  , red1
  , red2
  , green1
  , green2
  , yellow1
  , yellow2
  , blue1
  , blue2
  , magenta1
  , magenta2
  , cyan1
  , cyan2
  , white1
  , white2
  , colorN
  , colorF
  , gapBase
  , gapFull
  , border
  , tabTheme
  , promptTheme
  , hotPromptTheme
  )
where

import           Data.Char
import           Data.Function
import           Data.List
import           Graphics.X11.Xlib.Types
import           XMonad.Layout.Decoration
import qualified XMonad.Prompt                 as P
import           Data.Ratio

font :: String
font = "xft:PragmataProMono:style=Regular:size=9:antialias=true"

black1, black2 :: String
(black1, black2) = ("#0b0806", "#2f2b2a")

-- | Red
red1, red2 :: String
(red1, red2) = ("#844d2c", "#a64848")

-- | Green
green1, green2 :: String
(green1, green2) = ("#57553a", "#897f5a")

-- | Yellow
yellow1, yellow2 :: String
(yellow1, yellow2) = ("#a17c38", "#c8b38d")

-- | Blue
blue1, blue2 :: String
(blue1, blue2) = ("#41434f", "#526274")

-- | Magenta
magenta1, magenta2 :: String
(magenta1, magenta2) = ("#6b4444", "#755c47")

-- | Cyan
cyan1, cyan2 :: String
(cyan1, cyan2) = ("#59664c", "#718062")

-- | White
white1, white2 :: String
(white1, white2) = ("#a19782", "#c1ab83")

colorN, colorF :: String
colorN = black2
colorF = white2

gapBase, gapFull :: Int
gapBase = 6
gapFull = gapBase * 2

height, border :: Dimension
height = 12 * 2
border = 1

tabTheme :: Theme
tabTheme = def { activeColor         = black1
               , inactiveColor       = black2
               , urgentColor         = red1
               , activeBorderColor   = white1
               , inactiveBorderColor = white2
               , urgentBorderColor   = red2
               , activeTextColor     = white1
               , inactiveTextColor   = white2
               , urgentTextColor     = red2
               , fontName            = font
               , decoHeight          = height
               }

promptTheme, hotPromptTheme :: P.XPConfig
promptTheme = def
  { P.font              = font
  , P.bgColor           = black1
  , P.fgColor           = white1
  , P.fgHLight          = white2
  , P.bgHLight          = black2
  , P.borderColor       = white2
  , P.promptBorderWidth = border
  , P.position = P.CenteredAt { P.xpCenterY = 3 % 10, P.xpWidth = 9 % 10 }
  , P.height            = height
  , P.maxComplRows      = Just 5
  , P.searchPredicate   = isInfixOf `on` map toLower
  , P.alwaysHighlight   = True
  }
hotPromptTheme = promptTheme { P.bgColor  = black2
                             , P.fgColor  = white2
                             , P.fgHLight = white1
                             , P.bgHLight = black1
                             }
