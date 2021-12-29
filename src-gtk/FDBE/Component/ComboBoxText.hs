{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels      #-}
{-# LANGUAGE OverloadedLists       #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TypeFamilies          #-}

-- | A combobox that allow choosing some text from a number of options.
module FDBE.Component.ComboBoxText
  ( ComboBoxText(..)
  , comboBoxText
  ) where

import           FDBE.Prelude

import qualified GI.Gtk                        as Gtk
import           GI.Gtk.Declarative
import           GI.Gtk.Declarative.Component

data ComboBoxText event = ComboBoxText
  { choices :: Vector Text
  , position :: Maybe Int
  , onChanged :: Maybe (Int -> event)
  , rawAttributes :: forall e. Vector (Attribute Gtk.ComboBoxText e)
  }

-- | Default ComboBoxText
comboBoxText :: ComboBoxText event
comboBoxText = ComboBoxText
  { choices = []
  , position = Nothing
  , onChanged = Nothing
  , rawAttributes = []
  }

instance Component ComboBoxText where

  data ComponentState ComboBoxText = ComboBoxTextState

  data ComponentAction ComboBoxText = ValueChanged Int

  createComponent _decl =
    (ComboBoxTextState, Nothing)
  
  patchComponent state _decl =
    state
  
  update ComboBoxText{..} = \case
    ValueChanged pos -> do
      forM_ onChanged $ \f ->
        updateParent (f pos)
  
  view ComboBoxText{..} _state =
    widget Gtk.ComboBoxText $
      [ onM changed (fmap (ValueChanged . fromIntegral) . #getActive)
      , customAttribute () (ComboBoxTextAttr choices position)
      ] <> rawAttributes
    where
      changed =
        #changed :: Gtk.SignalProxy Gtk.ComboBoxText Gtk.ComboBoxChangedSignalInfo

data ComboBoxTextAttr event = ComboBoxTextAttr
  { attrChoices :: Vector Text
  , attrPosition :: Maybe Int
  }

instance CustomAttribute Gtk.ComboBoxText ComboBoxTextAttr where

  data AttrState ComboBoxTextAttr = ComboBoxTextAttrState

  attrCreate _ctx combo ComboBoxTextAttr{..} = do
    forM_ attrChoices (#appendText combo)
    updatePos combo attrPosition
    pure ComboBoxTextAttrState

  attrPatch _ctx combo state old new = do
    let choiceChange = attrChoices old /= attrChoices new
        positionChange = attrPosition old /= attrPosition new
    when choiceChange $ do
      #removeAll combo
      forM_ (attrChoices new) (#appendText combo)
    when (choiceChange || positionChange) $
      updatePos combo (attrPosition new)
    pure state

updatePos :: Gtk.ComboBoxText -> Maybe Int -> IO ()
updatePos combo position =
  #setActive combo $ maybe (-1) fromIntegral position
