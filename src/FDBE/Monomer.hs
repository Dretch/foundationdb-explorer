{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}
{- Helpers to make Monomer easier to use -}
module FDBE.Monomer
( stdSpacer
, intersperseSpacers
, adwaitaTheme
, compactTheme
) where

import FDBE.Prelude
import Data.List (intersperse)
import Monomer
import Control.Lens
import qualified Monomer.Core.Lens as L
import qualified Monomer.Core.Themes.BaseTheme as T

stdSpacer :: WidgetNode s e
stdSpacer = spacer_ [width 2] -- todo: make feature request asking for cell margin instead

intersperseSpacers :: [WidgetNode s e] -> [WidgetNode s e]
intersperseSpacers = intersperse stdSpacer

adwaitaTheme :: Theme
adwaitaTheme = compactTheme $ T.baseTheme lightThemeColors {
  T.clearColor = rgbHex "#ededed",
  T.btnBgBasic = rgbHex "#fefefe",
  T.btnBgHover = rgbHex "#f7f6f6",
  T.btnBgFocus = rgbHex "#f7f6f6",
  T.btnBgActive = rgbHex "#e7e7e7",
  T.btnBgDisabled = rgbHex "#d8d8d8",
  T.btnText = rgbHex "#1c1c1c",
  T.btnTextDisabled = rgbHex "#c7c8c6",
  T.btnMainText = rgbHex "#ffffff",
  T.btnMainBgBasic = rgbHex "#6a9acd",
  T.btnMainBgDisabled = rgbHex "#a8c1df",
  T.inputBorder = rgbHex "#babdb6",
  T.inputFocusBorder = rgbHex "#729fcf",
  T.inputBgBasic = rgbHex "#ffffff",
  T.inputSelFocus = rgbHex "#5a9adc"
}

compactTheme :: Theme -> Theme
compactTheme t = t
    & L.basic      %~ fixThemeState
    & L.hover      %~ fixThemeState
    & L.focus      %~ fixThemeState
    & L.focusHover %~ fixThemeState
    & L.active     %~ fixThemeState
    & L.disabled   %~ fixThemeState
  where
    fontSize :: FontSize
    fontSize = FontSize 12

    fixThemeState :: ThemeState -> ThemeState
    fixThemeState ts = ts
      & L.labelStyle                %~ fixStyleState
      & L.textFieldStyle            %~ fixStyleState . fixTextInput
      & L.btnStyle                  %~ fixStyleState . fixBtnStyle
      & L.btnMainStyle              %~ fixStyleState . fixBtnStyle
      & L.dropdownStyle             %~ fixStyleState . fixTextInput
      & L.dropdownListStyle         %~ fixStyleState
      & L.dropdownItemStyle         %~ fixStyleState . fixDropdownListItem
      & L.dropdownItemSelectedStyle %~ fixStyleState . fixDropdownListItem
      & L.numericFieldStyle         %~ fixStyleState . fixTextInput
      & L.tooltipStyle              %~ fixStyleState
      & L.checkboxWidth             .~ 14

    fixStyleState :: StyleState -> StyleState
    fixStyleState ss = ss
      & L.text . non def . L.fontSize ?~ fontSize
    
    fixBtnStyle :: StyleState -> StyleState
    fixBtnStyle ss = ss
      & L.padding ?~ padding 4
      & L.radius  ?~ radius 3
    
    fixTextInput :: StyleState -> StyleState
    fixTextInput ss = ss
      & L.padding ?~ padding 4
      & L.radius  ?~ radius 3

    fixDropdownListItem :: StyleState -> StyleState
    fixDropdownListItem ss = ss
      & L.padding ?~ padding 4
