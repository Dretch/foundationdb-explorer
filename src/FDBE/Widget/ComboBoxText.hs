{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedLabels      #-}
{-# LANGUAGE OverloadedLists       #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TypeFamilies          #-}

-- | A combobox that allow choosing some text from a number of options.
module FDBE.Widget.ComboBoxText
  ( ComboBoxAttribute(..)
  , comboBox
  ) where

import           Control.Monad      (forM_, when)
import           Data.Text          (Text)
import           Data.Vector        (Vector)
import qualified Data.Vector        as Vector
import qualified GI.Gtk             as Gtk
import           GI.Gtk.Declarative

data ComboBoxAttribute event
  = RawAttribute (Attribute Gtk.ComboBoxText event)
  | Choices [Text]
  | Position Int
  | OnChanged (Int -> event)

comboBox :: Vector (ComboBoxAttribute event) -> Widget event
comboBox attrs =
  widget Gtk.ComboBoxText $
    Vector.fromList rawAttrs `Vector.snoc` customAttribute () comboBoxText
  where
    (comboBoxText, rawAttrs) = foldl go (defaultComboBoxText, []) attrs
    go (comboBoxText', attrs') = \case
      RawAttribute a  -> (comboBoxText', a : attrs')
      Choices choices -> (comboBoxText' { choices }, attrs')
      Position p      -> (comboBoxText' { position = Just p }, attrs')
      OnChanged f     -> (comboBoxText', onM changed (fmap (f . fromIntegral) . #getActive) : attrs')
    defaultComboBoxText = ComboBoxText
      { choices = []
      , position = Nothing
      }
    changed =
      #changed :: Gtk.SignalProxy Gtk.ComboBoxText Gtk.ComboBoxChangedSignalInfo

data ComboBoxText event = ComboBoxText
  { choices  :: [Text]
  , position :: Maybe Int
  }
  deriving (Functor)

instance CustomAttribute Gtk.ComboBoxText ComboBoxText where

  data AttrState ComboBoxText = ComboBoxTextState

  attrCreate combo ComboBoxText {..} = do
    forM_ choices (#appendText combo)
    updatePos combo position
    pure ComboBoxTextState

  attrPatch combo state old new = do
    let choiceChange = choices old /= choices new
        positionChange = position old /= position new
    when choiceChange $ do
      #removeAll combo
      forM_ (choices new) (#appendText combo)
    when (choiceChange || positionChange) $
      updatePos combo (position new)
    pure state

  attrDestroy _combo _state _decl =
    pure ()

  attrSubscribe _combo _state _decl =
    mempty

updatePos :: Gtk.ComboBoxText -> Maybe Int -> IO ()
updatePos combo position =
  #setActive combo $ maybe (-1) fromIntegral position
