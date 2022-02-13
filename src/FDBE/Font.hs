module FDBE.Font
  ( regular,
    bold,
    mono,
    monoBold,
    fontDefs,
  )
where

import Monomer (AppConfig, Font (unFont), appFontDef)

regular :: Font
regular = "Regular"

bold :: Font
bold = "Bold"

mono :: Font
mono = "Mono"

monoBold :: Font
monoBold = "MonoBold"

fontDefs :: [AppConfig e]
fontDefs =
  [ appFontDef (unFont regular) "./assets/fonts/Cantarell/Cantarell-Regular.ttf",
    appFontDef (unFont bold) "./assets/fonts/Cantarell/Cantarell-Bold.ttf",
    appFontDef (unFont mono) "./assets/fonts/RobotoMono/RobotoMono-Regular.ttf",
    appFontDef (unFont monoBold) "./assets/fonts/RobotoMono/RobotoMono-Bold.ttf"
  ]
