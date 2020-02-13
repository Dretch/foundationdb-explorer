{-# LANGUAGE OverloadedStrings #-}

module FDBE.Widget.ComboBoxBool
  ( comboBoxBool
  ) where

import           Data.Bool                (bool)
import           Data.Vector              (Vector)
import qualified GI.Gtk                   as Gtk
import           GI.Gtk.Declarative

import           FDBE.Widget.ComboBoxText (comboBoxText)

comboBoxBool
  :: Vector (Attribute Gtk.ComboBoxText event)
  -> Maybe Bool
  -> Maybe (Bool -> event)
  -> Widget event
comboBoxBool attrs value onChange =
  comboBoxText
    attrs
    ["False", "True"]
    (bool 0 1 <$> value)
    ((. (/= 0)) <$> onChange)
