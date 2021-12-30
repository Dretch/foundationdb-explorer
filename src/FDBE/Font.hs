module FDBE.Font
( regular
, mono
, monoBold
, fontDefs
) where

import Monomer

regular :: Font
regular = "Regular"

mono :: Font
mono = "Mono"

monoBold :: Font
monoBold = "MonoBold"

fontDefs :: [AppConfig e]
fontDefs =
  [ appFontDef (unFont regular) "./assets/fonts/Roboto/Roboto-Regular.ttf"
  , appFontDef (unFont mono) "./assets/fonts/RobotoMono/RobotoMono-Regular.ttf"
  , appFontDef (unFont monoBold) "./assets/fonts/RobotoMono/RobotoMono-Bold.ttf"
  ]