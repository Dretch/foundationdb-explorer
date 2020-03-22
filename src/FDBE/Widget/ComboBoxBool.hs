{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module FDBE.Widget.ComboBoxBool
  ( ComboBoxAttribute(..)
  , comboBox
  ) where

import           FDBE.Prelude

import qualified Data.Vector              as Vector
import qualified GI.Gtk                   as Gtk
import           GI.Gtk.Declarative

import qualified FDBE.Widget.ComboBoxText as ComboBoxText

data ComboBoxAttribute event
  = RawAttribute (Attribute Gtk.ComboBoxText event)
  | Position Bool
  | OnChanged (Bool -> event)

comboBox :: Vector (ComboBoxAttribute event) -> Widget event
comboBox attrs =
  ComboBoxText.comboBox $
    (mapAttr <$> attrs) `Vector.snoc` ComboBoxText.Choices ["False", "True"]
  where
    mapAttr = \case
      RawAttribute a -> ComboBoxText.RawAttribute a
      Position b     -> ComboBoxText.Position $ if b then 1 else 0
      OnChanged f    -> ComboBoxText.OnChanged $ f . (/= 0)
