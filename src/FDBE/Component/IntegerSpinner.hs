{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE NamedFieldPuns            #-}
{-# LANGUAGE OverloadedLabels          #-}
{-# LANGUAGE OverloadedLists           #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TypeApplications          #-}
{-# LANGUAGE TypeFamilies              #-}

module FDBE.Component.IntegerSpinner
  ( IntegerSpinner(..)
  , integerSpinner
  ) where

import           FDBE.Prelude                  hiding (max, min)

import qualified GI.Gtk                        as Gtk
import           GI.Gtk.Declarative
import           GI.Gtk.Declarative.Component

data IntegerSpinner val event = IntegerSpinner
  { rawAttributes :: forall e. Vector (Attribute Gtk.SpinButton e)
  , value :: val
  , min :: val
  , max :: val
  , onChanged :: Maybe (val -> event)
  }

integerSpinner :: (Integral val, Bounded val) => IntegerSpinner val event
integerSpinner = IntegerSpinner
  { rawAttributes = []
  , value = 0
  , min = minBound
  , max = maxBound
  , onChanged = Nothing
  }

instance (Integral val, Typeable val) => Component (IntegerSpinner val) where

  data ComponentState (IntegerSpinner val) = IntegerSpinnerState

  data ComponentAction (IntegerSpinner val) = ValueChanged val

  createComponent _decl =
    (IntegerSpinnerState, Nothing)
  
  patchComponent state _decl =
    state

  update IntegerSpinner{..} = \case
    ValueChanged x ->
      forM_ onChanged $ \f ->
        updateParent (f x)

  view IntegerSpinner{..} _state =
    let attr = IntegerSpinnerAttr
          { attrValue = fromIntegral value
          , attrMin = fromIntegral min
          , attrMax = fromIntegral max
          }
    in
    widget Gtk.SpinButton $
      [ customAttribute () attr
      , onM #valueChanged (fmap (ValueChanged . truncate) . #getValue)
      ] <> rawAttributes

data IntegerSpinnerAttr event = IntegerSpinnerAttr
  { attrMin   :: Integer
  , attrMax   :: Integer
  , attrValue :: Integer
  }
  deriving (Eq)

instance CustomAttribute Gtk.SpinButton IntegerSpinnerAttr where

  data AttrState IntegerSpinnerAttr = IntegerSpinnerAttrState

  attrCreate _ctx spin attr = do
    setLimits spin attr
    Gtk.spinButtonSetDigits spin 0
    Gtk.spinButtonSetNumeric spin True
    pure IntegerSpinnerAttrState

  attrPatch _ctx spin state attr1 attr2 = do
    if attrMin attr1 /= attrMin attr2 || attrMax attr1 /= attrMax attr2 then
      setLimits spin attr2
    else if attrValue attr1 /= attrValue attr2 then
      void . Gtk.spinButtonSetValue spin $ fromIntegral @Integer $ attrValue attr2
    else
      pure ()
    pure state

setLimits :: Gtk.SpinButton -> IntegerSpinnerAttr event -> IO ()
setLimits spin IntegerSpinnerAttr {attrMin, attrMax, attrValue} = do
  adj <- Gtk.adjustmentNew
    (fromIntegral attrValue)
    (fromIntegral attrMin)
    (fromIntegral attrMax)
    10 0 0
  Gtk.spinButtonSetAdjustment spin adj
