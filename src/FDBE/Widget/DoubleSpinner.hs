{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels      #-}
{-# LANGUAGE OverloadedLists       #-}
{-# LANGUAGE TypeFamilies          #-}

module FDBE.Widget.DoubleSpinner (doubleSpinner) where

import           Control.Monad      (void, when)
import           Data.Vector        (Vector)
import qualified GI.Gtk             as Gtk
import           GI.Gtk.Declarative

doubleSpinner
  :: Vector (Attribute Gtk.SpinButton event)
  -> Double
  -> (Double -> event)
  -> Widget event
doubleSpinner attrs value onChange =
  widget Gtk.SpinButton
    $ attrs <> [listener, customAttribute $ DoubleSpinner value]
  where
    listener =
      onM #valueChanged (fmap onChange . #getValue)

data DoubleSpinner event = DoubleSpinner Double
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
    when (old /= new) $ do
      void $ Gtk.spinButtonSetValue spin new
    pure state

  attrDestroy _ _ _ =
    pure ()

  attrSubscribe _ _ _ _ =
    mempty
