{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels      #-}
{-# LANGUAGE OverloadedLists       #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}

-- | A multi-line text editor
module FDBE.Component.TextView
  ( TextView(..)
  , textView
  ) where

import           FDBE.Prelude

import qualified GI.GObject                     as GI
import qualified GI.Gtk                         as Gtk
import           GI.Gtk.Declarative
import           GI.Gtk.Declarative.Components
import           GI.Gtk.Declarative.EventSource (fromCancellation)

data TextView event = TextView
  { value :: Text
  , onChanged :: Maybe (Text -> event)
  , rawAttributes :: forall e. Vector (Attribute Gtk.TextView e)
  }

textView :: TextView event
textView = TextView
  { value = ""
  , onChanged = Nothing 
  , rawAttributes = []
  }

instance Component TextView where

  data ComponentState TextView = TextViewState

  data ComponentAction TextView = ValueChanged Text

  createComponent _decl =
    (TextViewState, Nothing)
  
  patchComponent state _decl =
    state
  
  update TextView{..} = \case
    ValueChanged x -> do
      forM_ onChanged $ \f ->
        updateParent (f x)
  
  view (TextView{..} :: TextView e) _state =
    bin Gtk.ScrolledWindow [ #hexpand := True, #shadowType := Gtk.ShadowTypeEtchedIn ] $
      widget Gtk.TextView $
        [ #valign := Gtk.AlignFill
        , customAttribute () (TextViewAttr value ValueChanged)
        ] <> rawAttributes

data TextViewAttr event = TextViewAttr
  { attrValue     :: Text
  , attrOnChanged :: Text -> event
  }

instance CustomAttribute Gtk.TextView TextViewAttr where

  data AttrState TextViewAttr = TextViewAttrState

  attrCreate _ctx view' TextViewAttr{..} = do
    buffer <- Gtk.textViewGetBuffer view'
    Gtk.setTextBufferText buffer attrValue
    pure TextViewAttrState

  attrPatch _ctx view' _state old new = do
    when (attrValue old /= attrValue new) $ do
      buffer <- Gtk.textViewGetBuffer view'
      curValue <- Gtk.getTextBufferText buffer
      -- setting the value resets the cursor to the end, which sucks for
      -- the user, so avoid doing it unless we really need to
      when (fromMaybe "" curValue /= attrValue new) $
        Gtk.setTextBufferText buffer (attrValue new)
    pure TextViewAttrState

  attrSubscribe _ctx view' _state TextViewAttr{..} cb = do
    buffer <- Gtk.textViewGetBuffer view'
    h <- Gtk.on buffer #changed (cb . attrOnChanged . fromMaybe "" =<< Gtk.getTextBufferText buffer)
    pure (fromCancellation (GI.signalHandlerDisconnect buffer h))
