{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels      #-}
{-# LANGUAGE OverloadedLists       #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TypeFamilies          #-}

-- | A combobox that allow choosing some text from a number of options.
module FDBE.Widget.ComboBoxText (comboBoxText) where

import           Control.Monad      (forM_, when)
import           Data.Text          (Text)
import           Data.Vector        (Vector)
import qualified GI.Gtk             as Gtk
import           GI.Gtk.Declarative

comboBoxText
  :: Vector (Attribute Gtk.ComboBoxText event)
  -> [Text]
  -> Maybe Int
  -> Maybe (Int -> event)
  -> Widget event
comboBoxText attrs choices position listener =
  widget Gtk.ComboBoxText
   $ attrs <> listenerAttr <> [customAttribute $ ComboBoxText choices position]
  where
    listenerAttr
      | Just h <- listener =
        [onM #changed (fmap (h . fromIntegral) . #getActive)]
      | otherwise =
        []

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
    when (choiceChange || positionChange) $ do
      updatePos combo (position new)
    pure state

  attrDestroy _combo _state _decl =
    pure ()

  attrSubscribe _combo _state _decl =
    mempty

updatePos :: Gtk.ComboBoxText -> Maybe Int -> IO ()
updatePos combo position =
  #setActive combo $ maybe (-1) fromIntegral position
