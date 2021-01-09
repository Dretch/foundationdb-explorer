{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}

module FDBE.Component.ComboBoxBool
  ( comboBoxBool
  ) where

import           FDBE.Prelude

import qualified FDBE.Component.ComboBoxText as ComboBoxText

comboBoxBool :: Bool -> Maybe (Bool -> event) -> ComboBoxText.ComboBoxText event
comboBoxBool position onChanged =
  ComboBoxText.comboBoxText
    { ComboBoxText.choices = ["False", "True"]
    , ComboBoxText.position = Just (if position then 1 else 0)
    , ComboBoxText.onChanged = (. (/= 0)) <$> onChanged
    }
