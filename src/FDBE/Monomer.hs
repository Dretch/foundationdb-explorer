{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}
{- Helpers to make Monomer easier to use -}
module FDBE.Monomer
( stdSpacer
, intersperseSpacers
, compactTheme
) where

import FDBE.Prelude
import Data.List (intersperse)
import Monomer
import Control.Lens
import qualified Monomer.Core.Lens as L

stdSpacer :: WidgetNode s e
stdSpacer = spacer_ [width 2] -- todo: make feature request asking for cell margin instead

intersperseSpacers :: [WidgetNode s e] -> [WidgetNode s e]
intersperseSpacers = intersperse stdSpacer

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
      & L.radius  ?~ radius 2
    
    fixTextInput :: StyleState -> StyleState
    fixTextInput ss = ss
      & L.padding ?~ padding 4
      & L.radius  ?~ radius 2

    fixDropdownListItem :: StyleState -> StyleState
    fixDropdownListItem ss = ss
      & L.padding ?~ padding 4
