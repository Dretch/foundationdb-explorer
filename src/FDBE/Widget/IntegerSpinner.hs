{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedLabels      #-}
{-# LANGUAGE OverloadedLists       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}

module FDBE.Widget.IntegerSpinner (integerSpinner) where

import           Control.Monad      (void)
import           Data.Vector        (Vector)
import qualified GI.Gtk             as Gtk
import           GI.Gtk.Declarative

integerSpinner
  :: Vector (Attribute Gtk.SpinButton event)
  -> Bool
  -> Integer
  -> (Integer -> event)
  -> Widget event
integerSpinner attrs allowNegative value onChange =
  widget Gtk.SpinButton
    $ attrs <> [listener, customAttribute $ IntegerSpinner allowNegative value]
  where
    listener =
      onM #valueChanged (fmap (onChange . truncate) . #getValue)

data IntegerSpinner event = IntegerSpinner Bool Integer
  deriving (Functor)

instance CustomAttribute Gtk.SpinButton IntegerSpinner where

  data AttrState IntegerSpinner = IntegerSpinnerState

  attrCreate spin (IntegerSpinner allowNegative value) = do
    setLimits spin value allowNegative
    Gtk.spinButtonSetDigits spin 0
    Gtk.spinButtonSetNumeric spin True
    pure IntegerSpinnerState

  attrPatch spin state (IntegerSpinner allowNegative1 value1) (IntegerSpinner allowNegative2 value2) = do
    if allowNegative1 /= allowNegative2 then do
      setLimits spin value2 allowNegative2
    else if value2 /= value1 then do
      void . Gtk.spinButtonSetValue spin $ fromIntegral value2
    else do
      pure ()
    pure state

  attrDestroy _ _ _ =
    pure ()

  attrSubscribe _ _ _ _ =
    mempty

setLimits :: Gtk.SpinButton -> Integer -> Bool -> IO ()
setLimits spin value allowNegative = do
  let minLimit = if allowNegative then fromIntegral (minBound :: Int) else 0
      maxLimit = fromIntegral (maxBound :: Int)
  adj <- Gtk.adjustmentNew (fromIntegral value) minLimit maxLimit 10 0 0
  Gtk.spinButtonSetAdjustment spin adj