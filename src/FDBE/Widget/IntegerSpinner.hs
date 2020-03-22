{-# LANGUAGE DeriveFunctor             #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE NamedFieldPuns            #-}
{-# LANGUAGE OverloadedLabels          #-}
{-# LANGUAGE OverloadedLists           #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TypeFamilies              #-}

module FDBE.Widget.IntegerSpinner
  ( SpinnerAttribute(..)
  , spinner
  ) where

import           FDBE.Prelude       hiding (max, min)

import qualified Data.Vector        as Vector
import qualified GI.Gtk             as Gtk
import           GI.Gtk.Declarative

data SpinnerAttribute val event
  = RawAttribute (Attribute Gtk.SpinButton event)
  | Value val
  | MinValue val
  | MaxValue val
  | (Bounded val, Integral val) => OnChanged (val -> event)

spinner
  :: forall val event. (Integral val, Bounded val)
  => Vector (SpinnerAttribute val event)
  -> Widget event
spinner attrs =
  widget Gtk.SpinButton $
    Vector.fromList rawAttrs `Vector.snoc` customAttribute () (IntegerSpinner props)
  where
    (props, rawAttrs) = foldl go (defaultProps, []) attrs
    go (props', attrs') = \case
      RawAttribute a -> (props', a : attrs')
      Value value    -> (props' { value = fromIntegral value }, attrs')
      MinValue min   -> (props' { min = fromIntegral min }, attrs')
      MaxValue max   -> (props' { max = fromIntegral max }, attrs')
      OnChanged f    -> (props', onM #valueChanged (fmap (f . truncate) . #getValue) : attrs')
    defaultProps = IntegerSpinnerProperties
      { min = fromIntegral (minBound :: val)
      , max = fromIntegral (maxBound :: val)
      , value = 0
      }

data IntegerSpinnerProperties = IntegerSpinnerProperties
  { min   :: Integer
  , max   :: Integer
  , value :: Integer
  }
  deriving (Eq)

newtype IntegerSpinner event = IntegerSpinner IntegerSpinnerProperties
  deriving (Functor)

instance CustomAttribute Gtk.SpinButton IntegerSpinner where

  data AttrState IntegerSpinner = IntegerSpinnerState

  attrCreate spin (IntegerSpinner props) = do
    setLimits spin props
    Gtk.spinButtonSetDigits spin 0
    Gtk.spinButtonSetNumeric spin True
    pure IntegerSpinnerState

  attrPatch spin state (IntegerSpinner props1) (IntegerSpinner props2) = do
    if min props1 /= min props2 || max props1 /= max props2 then
      setLimits spin props2
    else if value props1 /= value props2 then
      void . Gtk.spinButtonSetValue spin $ fromIntegral $ value props2
    else
      pure ()
    pure state

  attrDestroy _ _ _ =
    pure ()

  attrSubscribe _ _ _ _ =
    mempty

setLimits :: Gtk.SpinButton -> IntegerSpinnerProperties -> IO ()
setLimits spin IntegerSpinnerProperties {min, max, value} = do
  adj <- Gtk.adjustmentNew
    (fromIntegral value)
    (fromIntegral min)
    (fromIntegral max)
    10 0 0
  Gtk.spinButtonSetAdjustment spin adj
