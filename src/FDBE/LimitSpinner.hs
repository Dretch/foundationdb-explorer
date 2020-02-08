{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedLabels      #-}
{-# LANGUAGE OverloadedLists       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}

module FDBE.LimitSpinner (limitSpinner) where

import           Control.Monad                  (when, void)
import qualified GI.GObject                     as GI
import qualified GI.Gtk                         as Gtk
import           GI.Gtk.Declarative
import           GI.Gtk.Declarative.EventSource (fromCancellation)

import           FDBE.Event                     (Event (..))
import           FDBE.State                     (SearchRange (..))

data LimitSpinner event = LimitSpinner SearchRange (SearchRange -> event)
  deriving (Functor)

instance CustomAttribute Gtk.SpinButton LimitSpinner where

  data AttrState LimitSpinner = LimitSpinnerState

  attrCreate spin (LimitSpinner searchRange _) = do
    adj <- Gtk.adjustmentNew (fromIntegral $ searchLimit searchRange) 0 1000000 10 0 0
    Gtk.spinButtonSetAdjustment spin adj
    Gtk.spinButtonSetDigits spin 0
    Gtk.spinButtonSetNumeric spin True
    pure LimitSpinnerState

  attrPatch spin state (LimitSpinner old _) (LimitSpinner new _) = do
    when (searchLimit old /= searchLimit new) $ do
      void . Gtk.spinButtonSetValue spin $ fromIntegral $ searchLimit new
    pure state

  attrDestroy _ _ _ =
    pure ()

  attrSubscribe spin _v (LimitSpinner searchRange setSearchRange) cb = do
    h <- Gtk.on spin #valueChanged $ do
      value <- #getValue spin
      cb . setSearchRange $ searchRange { searchLimit = truncate value }
    pure . fromCancellation $ GI.signalHandlerDisconnect spin h

limitSpinner :: SearchRange -> Widget Event
limitSpinner searchRange =
  widget Gtk.SpinButton [customAttribute $ LimitSpinner searchRange SetSearchRange]
