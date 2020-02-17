{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedLabels      #-}
{-# LANGUAGE OverloadedLists       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}

module FDBE.Widget.IntegerSpinner
  ( integerSpinner
  , wordSpinner
  , word16Spinner
  , word64Spinner
  ) where

import           Control.Monad      (void)
import           Data.Vector        (Vector)
import           Data.Word          (Word, Word16, Word64)
import qualified GI.Gtk             as Gtk
import           GI.Gtk.Declarative
import           Prelude            hiding (max, min)

integerSpinner
  :: Vector (Attribute Gtk.SpinButton event)
  -> Integer
  -> (Integer -> event)
  -> Widget event
integerSpinner attrs value onChange =
  spinner
    attrs
    (IntegerSpinnerProperties min max)
    value
    onChange
  where
    min = fromIntegral (minBound :: Int)
    max = fromIntegral (maxBound :: Int)

wordSpinner
  :: Vector (Attribute Gtk.SpinButton event)
  -> Word
  -> (Word -> event)
  -> Widget event
wordSpinner = spinner_

word16Spinner
  :: Vector (Attribute Gtk.SpinButton event)
  -> Word16
  -> (Word16 -> event)
  -> Widget event
word16Spinner = spinner_

word64Spinner
  :: Vector (Attribute Gtk.SpinButton event)
  -> Word64
  -> (Word64 -> event)
  -> Widget event
word64Spinner = spinner_

spinner_
  :: forall i event. (Integral i, Bounded i)
  => Vector (Attribute Gtk.SpinButton event)
  -> i
  -> (i -> event)
  -> Widget event
spinner_ attrs value onChange =
  spinner
    attrs
    (IntegerSpinnerProperties min max)
    (fromIntegral value)
    (onChange . fromIntegral)
  where
    min = fromIntegral (minBound :: i)
    max = fromIntegral (maxBound :: i)

spinner
  :: Vector (Attribute Gtk.SpinButton event)
  -> IntegerSpinnerProperties
  -> Integer
  -> (Integer -> event)
  -> Widget event
spinner attrs props value onChange =
  widget Gtk.SpinButton
    $ attrs <> [listener, customAttribute $ IntegerSpinner props value]
  where
    listener =
      onM #valueChanged (fmap (onChange . truncate) . #getValue)

data IntegerSpinnerProperties = IntegerSpinnerProperties
  { min :: Integer
  , max :: Integer
  }
  deriving (Eq)

data IntegerSpinner event = IntegerSpinner IntegerSpinnerProperties Integer
  deriving (Functor)

instance CustomAttribute Gtk.SpinButton IntegerSpinner where

  data AttrState IntegerSpinner = IntegerSpinnerState

  attrCreate spin (IntegerSpinner props value) = do
    setLimits spin value props
    Gtk.spinButtonSetDigits spin 0
    Gtk.spinButtonSetNumeric spin True
    pure IntegerSpinnerState

  attrPatch spin state (IntegerSpinner props1 value1) (IntegerSpinner props2 value2) = do
    if props1 /= props2 then do
      setLimits spin value2 props2
    else if value2 /= value1 then do
      void . Gtk.spinButtonSetValue spin $ fromIntegral value2
    else do
      pure ()
    pure state

  attrDestroy _ _ _ =
    pure ()

  attrSubscribe _ _ _ _ =
    mempty

setLimits :: Gtk.SpinButton -> Integer -> IntegerSpinnerProperties -> IO ()
setLimits spin value IntegerSpinnerProperties {min, max} = do
  adj <- Gtk.adjustmentNew
    (fromIntegral value)
    (fromIntegral min)
    (fromIntegral max)
    10 0 0
  Gtk.spinButtonSetAdjustment spin adj
