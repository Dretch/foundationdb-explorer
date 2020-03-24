{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels      #-}
{-# LANGUAGE OverloadedLists       #-}
{-# LANGUAGE TypeFamilies          #-}

module FDBE.Widget.DoubleSpinner
  ( SpinnerAttribute(..)
  , spinner
  ) where

import           FDBE.Prelude

import qualified Data.Vector        as Vector
import qualified GI.Gtk             as Gtk
import           GI.Gtk.Declarative

data SpinnerAttribute event
  = RawAttribute (Attribute Gtk.SpinButton event)
  | Value Double
  | OnChanged (Double -> event)

spinner :: Vector (SpinnerAttribute event) -> Widget event
spinner attrs =
  widget Gtk.SpinButton $
    Vector.fromList rawAttrs `Vector.snoc` customAttribute () (DoubleSpinner value)
  where
    (value, rawAttrs) = foldl go (0, []) attrs
    go (value', attrs') = \case
      RawAttribute a -> (value', a : attrs')
      Value x        -> (x, attrs')
      OnChanged f    -> (value', onM #valueChanged (fmap f . #getValue) : attrs')

newtype DoubleSpinner event = DoubleSpinner Double
  deriving (Functor)

instance CustomAttribute Gtk.SpinButton DoubleSpinner where

  data AttrState DoubleSpinner = DoubleSpinnerState

  attrCreate spin (DoubleSpinner value) = do
    adj <- Gtk.adjustmentNew value (-100000000) 100000000 10 0 0
    Gtk.spinButtonSetAdjustment spin adj
    Gtk.spinButtonSetDigits spin 10
    Gtk.spinButtonSetNumeric spin True
    pure DoubleSpinnerState

  attrPatch spin state (DoubleSpinner old) (DoubleSpinner new) = do
    when (old /= new) $
      void $ Gtk.spinButtonSetValue spin new
    pure state
