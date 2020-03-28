{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedLabels      #-}
{-# LANGUAGE OverloadedLists       #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TypeFamilies          #-}

-- | A multi-line text editor
module FDBE.Widget.TextView
  ( TextViewAttribute(..)
  , textView
  ) where

import           FDBE.Prelude

import qualified Data.Vector                    as Vector
import qualified GI.GObject                     as GI
import qualified GI.Gtk                         as Gtk
import           GI.Gtk.Declarative
import           GI.Gtk.Declarative.EventSource (fromCancellation)

data TextViewAttribute event
  = RawAttribute (Attribute Gtk.TextView event)
  | Value Text
  | OnChanged (Text -> event)

textView :: Vector (TextViewAttribute event) -> Widget event
textView attrs =
  bin Gtk.ScrolledWindow [ #hexpand := True, #shadowType := Gtk.ShadowTypeEtchedIn ] $
    widget Gtk.TextView $
      Vector.fromList rawAttrs <> [#valign := Gtk.AlignFill, customAttribute () decl]
  where
    (decl, rawAttrs) = foldl go (defaultTextView, []) attrs
    go (decl', attrs') = \case
      RawAttribute a -> (decl', a : attrs')
      Value value    -> (decl' { value }, attrs')
      OnChanged f    -> (decl' { onChanged = f : onChanged decl' }, attrs')
    defaultTextView = TextView
      { value = ""
      , onChanged = []
      }

data TextView event = TextView
  { value     :: Text
  , onChanged :: [Text -> event]
  }
  deriving (Functor)

instance CustomAttribute Gtk.TextView TextView where

  data AttrState TextView = TextViewState

  attrCreate view TextView {..} = do
    buffer <- Gtk.textViewGetBuffer view
    Gtk.setTextBufferText buffer value
    pure TextViewState

  attrPatch view _state old new = do
    when (value old /= value new) $ do
      buffer <- Gtk.textViewGetBuffer view
      curValue <- Gtk.getTextBufferText buffer
      -- setting the value resets the cursor to the end, which sucks for
      -- the user, so avoid doing it unless we really need to
      when (fromMaybe "" curValue /= value new) $
        Gtk.setTextBufferText buffer (value new)
    pure TextViewState

  attrSubscribe view _state TextView {..} cb = do
    buffer <- Gtk.textViewGetBuffer view
    flip foldMap onChanged $ \listener -> do
      h <- Gtk.on buffer #changed (cb . listener . fromMaybe "" =<< Gtk.getTextBufferText buffer)
      pure (fromCancellation (GI.signalHandlerDisconnect buffer h))
