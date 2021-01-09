{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels      #-}
{-# LANGUAGE OverloadedLists       #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TypeFamilies          #-}

module FDBE.Component.DoubleSpinner
  ( DoubleSpinner(..)
  , doubleSpinner
  ) where

import           FDBE.Prelude

import qualified GI.Gtk                        as Gtk
import           GI.Gtk.Declarative
import           GI.Gtk.Declarative.Components

data DoubleSpinner event = DoubleSpinner
  { value :: Double
  , onChanged :: Maybe (Double -> event)
  , rawAttributes :: forall e. Vector (Attribute Gtk.SpinButton e)
  }

doubleSpinner :: DoubleSpinner event
doubleSpinner = DoubleSpinner
  { value = 0
  , onChanged = Nothing
  , rawAttributes = []
  }

instance Component DoubleSpinner where

  data ComponentState DoubleSpinner = DoubleSpinnerState

  data ComponentAction DoubleSpinner = ValueChanged Double

  createComponent _decl =
    (DoubleSpinnerState, Nothing)
  
  patchComponent state _decl =
    state
  
  update DoubleSpinner{..} = \case
    ValueChanged x ->
      forM_ onChanged $ \f ->
        updateParent (f x)
  
  view DoubleSpinner{..} _state =
    widget Gtk.SpinButton $
      [ onM #valueChanged (fmap ValueChanged . #getValue)
      , customAttribute () (DoubleSpinnerAttr value)
      ] <> rawAttributes

newtype DoubleSpinnerAttr event = DoubleSpinnerAttr Double

instance CustomAttribute Gtk.SpinButton DoubleSpinnerAttr where

  data AttrState DoubleSpinnerAttr = DoubleSpinnerAttrState

  attrCreate _ctx spin (DoubleSpinnerAttr value) = do
    adj <- Gtk.adjustmentNew value (-100000000) 100000000 10 0 0
    Gtk.spinButtonSetAdjustment spin adj
    Gtk.spinButtonSetDigits spin 10
    Gtk.spinButtonSetNumeric spin True
    pure DoubleSpinnerAttrState

  attrPatch _ctx spin state (DoubleSpinnerAttr old) (DoubleSpinnerAttr new) = do
    when (old /= new) $
      void $ Gtk.spinButtonSetValue spin new
    pure state
