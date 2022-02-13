{- Helpers to make Monomer easier to use -}
module FDBE.Monomer
  ( titleLabel,
    sizeReqUpdaterFlexMax,
    adwaitaTheme,
    compactTheme,
    useOldCompositeModel,
  )
where

import Control.Lens
import qualified FDBE.Font as Font
import FDBE.Prelude
import Monomer hiding (textColor)
import qualified Monomer.Core.Lens as L
import qualified Monomer.Core.Themes.BaseTheme as T

titleLabel :: Text -> WidgetNode s e
titleLabel s =
  label s `styleBasic` [textSize 14, textFont Font.bold, paddingV 8]

sizeReqUpdaterFlexMax :: CmbSizeReqUpdater t => t
sizeReqUpdaterFlexMax = sizeReqUpdater (bimap flexMax flexMax)
  where
    flexMax req = req {_szrFlex = 10000}

adwaitaTheme :: Theme
adwaitaTheme =
  compactTheme $
    T.baseTheme
      lightThemeColors
        { T.clearColor = clear,
          T.btnBgBasic = btnBg,
          T.btnBgHover = rgbHex "#f7f6f6",
          T.btnBgFocus = rgbHex "#f7f6f6",
          T.btnBgActive = rgbHex "#e7e7e7",
          T.btnBgDisabled = btnBgDisabled,
          T.btnText = text,
          T.btnTextDisabled = textDisabled,
          T.btnMainText = rgbHex "#ffffff",
          T.btnMainBgBasic = rgbHex "#4077b2",
          T.btnMainBgDisabled = rgbHex "#a8c1df",
          T.inputText = text,
          T.inputTextDisabled = textDisabled,
          T.inputBorder = inputBorder,
          T.inputFocusBorder = rgbHex "#729fcf",
          T.inputBgBasic = rgbHex "#f9f9f9",
          T.inputBgDisabled = btnBgDisabled,
          T.inputFgDisabled = textDisabled,
          T.inputSelFocus = rgbHex "#5a9adc",
          T.dialogBg = clear,
          T.iconBg = btnBg
        }
  where
    text = rgbHex "#1c1c1c"
    textDisabled = rgbHex "#96918c"
    inputBorder = rgbHex "#c9c6c2"
    btnBg = rgbHex "#fefefe"
    btnBgDisabled = rgbHex "#eeeeed"
    clear = rgbHex "#f2f1f0"

compactTheme :: Theme -> Theme
compactTheme t =
  t
    & L.basic %~ fixThemeState
    & L.hover %~ fixThemeState
    & L.focus %~ fixThemeState
    & L.focusHover %~ fixThemeState
    & L.active %~ fixThemeState
    & L.disabled %~ fixThemeState . fixDisabledThemeState
  where
    fontSize :: FontSize
    fontSize = FontSize 12

    fixThemeState :: ThemeState -> ThemeState
    fixThemeState ts =
      ts
        & L.labelStyle %~ fixStyleState
        & L.textFieldStyle %~ fixStyleState . fixTextInput
        & L.btnStyle %~ fixStyleState . fixBtnStyle
        & L.btnMainStyle %~ fixStyleState . fixBtnStyle
        & L.dropdownStyle %~ fixStyleState . fixTextInput
        & L.dropdownListStyle %~ fixStyleState
        & L.dropdownItemStyle %~ fixStyleState . fixDropdownListItem
        & L.dropdownItemSelectedStyle %~ fixStyleState . fixDropdownListItem
        & L.numericFieldStyle %~ fixStyleState . fixTextInput
        & L.tooltipStyle %~ fixStyleState
        & L.dialogFrameStyle %~ fixDialogFrameStyle
        & L.dialogCloseIconStyle %~ fixDialogCloseIconStyle
        & L.dialogButtonsStyle %~ fixDialogButtonsStyle
        & L.dialogTitleStyle %~ fixDialogTitleStyle
        & L.checkboxWidth .~ 14

    fixDisabledThemeState :: ThemeState -> ThemeState
    fixDisabledThemeState ts =
      ts
        -- make the drop-down arrows go gray when disabled: move upstream?
        & L.dropdownStyle . L.fgColor .~ (ts ^. L.checkboxStyle . L.fgColor)

    fixStyleState :: StyleState -> StyleState
    fixStyleState ss =
      ss
        & L.text . non def . L.fontSize ?~ fontSize

    fixBtnStyle :: StyleState -> StyleState
    fixBtnStyle ss =
      ss
        & L.padding ?~ padding 4
        & L.radius ?~ radius 3

    fixTextInput :: StyleState -> StyleState
    fixTextInput ss =
      ss
        & L.padding ?~ padding 4
        & L.radius ?~ radius 3

    fixDropdownListItem :: StyleState -> StyleState
    fixDropdownListItem ss =
      ss
        & L.padding ?~ padding 4

    fixDialogFrameStyle :: StyleState -> StyleState
    fixDialogFrameStyle ss =
      ss
        & L.radius ?~ radius 6

    fixDialogCloseIconStyle :: StyleState -> StyleState
    fixDialogCloseIconStyle ss =
      ss
        & L.radius ?~ radius 3
        & L.padding ?~ padding 2

    fixDialogButtonsStyle :: StyleState -> StyleState
    fixDialogButtonsStyle ss =
      ss
        & L.padding ?~ padding 4

    fixDialogTitleStyle :: StyleState -> StyleState
    fixDialogTitleStyle ss =
      ss
        & L.text . non def . L.fontSize ?~ FontSize 14
        & L.padding ?~ padding 4 <> paddingB 8

-- todo: skinny cursor!

useOldCompositeModel :: CompositeCfg s e sp ep
useOldCompositeModel =
  customModelBuilder $ \_parentModel oldModel _newModel -> oldModel
