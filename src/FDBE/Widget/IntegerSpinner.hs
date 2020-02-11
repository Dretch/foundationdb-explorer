{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedLabels      #-}
{-# LANGUAGE OverloadedLists       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}

module FDBE.Widget.IntegerSpinner (integerSpinner) where

import           Control.Monad      (void, when)
import           Data.Vector        (Vector)
import qualified GI.Gtk             as Gtk
import           GI.Gtk.Declarative

integerSpinner
  :: Vector (Attribute Gtk.SpinButton event)
  -> Integer
  -> (Integer -> event)
  -> Widget event
integerSpinner attrs value onChange =
  widget Gtk.SpinButton
    $ attrs <> [listener, customAttribute $ IntegerSpinner value onChange]
  where
    listener =
      onM #valueChanged (fmap (onChange . truncate) . #getValue)

data IntegerSpinner event = IntegerSpinner Integer (Integer -> event)
  deriving (Functor)

instance CustomAttribute Gtk.SpinButton IntegerSpinner where

  data AttrState IntegerSpinner = IntegerSpinnerState

  attrCreate spin (IntegerSpinner value _) = do
    adj <- Gtk.adjustmentNew (fromIntegral $ value) 0 1000000 10 0 0
    Gtk.spinButtonSetAdjustment spin adj
    Gtk.spinButtonSetDigits spin 0
    Gtk.spinButtonSetNumeric spin True
    pure IntegerSpinnerState

  attrPatch spin state (IntegerSpinner old _) (IntegerSpinner new _) = do
    when (old /= new) $ do
      void . Gtk.spinButtonSetValue spin $ fromIntegral new
    pure state

  attrDestroy _ _ _ =
    pure ()

  attrSubscribe _ _ _ _ =
    mempty
