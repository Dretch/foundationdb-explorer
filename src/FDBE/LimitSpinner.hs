{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedLabels    #-}
{-# LANGUAGE OverloadedLists     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module FDBE.LimitSpinner (limitSpinner) where

import           Control.Monad      (void)
import qualified GI.GObject                    as GI
import qualified GI.Gtk             as Gtk
import           GI.Gtk.Declarative
import           GI.Gtk.Declarative.EventSource ( fromCancellation )

import           FDBE.Event         (Event (..))
import           FDBE.State         (SearchRange (..))

limitSpinner :: SearchRange -> Widget Event
limitSpinner searchRange =
  Widget
    (CustomWidget
       { customWidget
       , customCreate
       , customPatch
       , customSubscribe
       , customAttributes
       , customParams
       })
  where
    customWidget = Gtk.SpinButton
    customCreate value' = do
      spin <- Gtk.spinButtonNewWithRange 0 1000000 100
      Gtk.spinButtonSetValue spin $ fromIntegral value'
      Gtk.spinButtonSetDigits spin 0
      Gtk.spinButtonSetNumeric spin True
      pure (spin, ())
    customPatch oldValue newValue ()
      | oldValue == newValue = CustomKeep
      | otherwise =
        CustomModify $ \spin ->
          void . Gtk.spinButtonSetValue spin $ fromIntegral newValue
    customAttributes = []
    customParams = searchLimit searchRange
    customSubscribe _value () (spin :: Gtk.SpinButton) cb = do
      h <- Gtk.on spin #valueChanged $ do
        value <- #getValue spin
        cb . SetSearchRange $ searchRange { searchLimit = truncate value }
      pure . fromCancellation $ GI.signalHandlerDisconnect spin h
