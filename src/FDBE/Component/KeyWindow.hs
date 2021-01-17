{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE OverloadedLabels           #-}
{-# LANGUAGE OverloadedLists            #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeFamilies               #-}

module FDBE.Component.KeyWindow
  ( KeyWindow(..)
  , KeyWindowId(..)
  ) where

import           FDBE.Prelude

import           Control.Exception                 (displayException)
import           Control.Monad.State               (modify)
import           Data.Text                         (pack)
import           FoundationDB                      (Database)
import           GI.Gtk                            (Align (..), Box (..),
                                                    Button (..), Frame (..),
                                                    Grid (..), Label (..),
                                                    Orientation (..))
import           GI.Gtk.Declarative
import           GI.Gtk.Declarative.Component
import           GI.Gtk.Declarative.Container.Grid (GridChild (..),
                                                    GridChildProperties (..),
                                                    defaultGridChildProperties)
import           System.Random (Random)

import           FDBE.FoundationDB                 (getKeyValue, setKeyValue, EditableBytes)
import           FDBE.State                        (Operation (..), operationSuccess)
import qualified FDBE.Component.ComboBoxText       as ComboBoxText
import qualified FDBE.Component.TupleEntry         as TupleEntry

newtype KeyWindowId = KeyWindowId UUID
  deriving (Generic)
  deriving newtype (Eq, Hashable, Random)

newtype KeyWindow event = KeyWindow Database

instance Component KeyWindow where

  data ComponentState KeyWindow = KeyWindowState
    { keyWindowKey      :: EditableBytes
    , keyWindowOldValue :: Operation (Maybe EditableBytes)
    , keyWindowNewValue :: Maybe EditableBytes
    , keyWindowSave     :: Operation ()
    }

  data ComponentAction KeyWindow
    = UpdateKeyWindowKey EditableBytes
    | LoadKeyWindowOldValue EditableBytes
    | UpdateKeyWindowOldValue (Operation (Maybe EditableBytes))
    | UpdateKeyWindowNewValue (Maybe EditableBytes)
    | KeyWindowSave EditableBytes (Maybe EditableBytes)
    | UpdateKeyWindowSave (Operation ())

  createComponent (KeyWindow _) =
    ( KeyWindowState
        { keyWindowKey = Left ""
        , keyWindowOldValue = OperationNotStarted
        , keyWindowNewValue = Nothing
        , keyWindowSave = OperationNotStarted
        }
    , Nothing
    )
  
  patchComponent state (KeyWindow _) =
    state
  
  update (KeyWindow database) = \case
    UpdateKeyWindowKey key ->
      modify $ \state -> state { keyWindowKey = key, keyWindowOldValue = OperationNotStarted }
    LoadKeyWindowOldValue key -> do
      modify $ \state -> state { keyWindowOldValue = OperationInProgress }
      updateIO $ do
        op <- getKeyValue database key >>= \case
          Left e    -> pure . OperationFailure . pack $ displayException e
          Right val -> pure $ OperationSuccess val
        pure . Just $ UpdateKeyWindowOldValue op
    UpdateKeyWindowOldValue op ->
      modify $ \state -> state { keyWindowOldValue = op }
    UpdateKeyWindowNewValue val ->
      modify $ \state -> state { keyWindowNewValue = val, keyWindowSave = OperationNotStarted }
    KeyWindowSave key value -> do
      modify $ \state -> state { keyWindowSave = OperationInProgress }
      updateIO $ do
        op <- setKeyValue database key value >>= \case
          Just e  -> pure . OperationFailure . pack $ displayException e
          Nothing -> pure $ OperationSuccess ()
        pure . Just $ UpdateKeyWindowSave op
    UpdateKeyWindowSave op ->
      modify $ \state -> state { keyWindowSave = op }
  
  view (KeyWindow _) KeyWindowState{..} =
    container Grid
      [ #orientation := OrientationVertical
      , #margin := 4
      , #rowHomogeneous := True
      , #columnHomogeneous := True
      ]
      [ GridChild
          { properties = defaultGridChildProperties { height = 2 }
          , child =
              bin Frame [#label := "Key", #vexpand := True, #hexpand := True, #margin := 2] $
                component TupleEntry.tupleEntry
                  { TupleEntry.value = keyWindowKey
                  , TupleEntry.onChanged = Just UpdateKeyWindowKey
                  }
          }
      , GridChild
          { properties = defaultGridChildProperties { leftAttach = 1 }
          , child =
              bin Frame
                [ #label := "Existing Value"
                , #hexpand := True
                , #margin := 2
                ] $
                oldValueEntry keyWindowKey keyWindowOldValue
          }
      , GridChild
          { properties = defaultGridChildProperties { leftAttach = 1, topAttach = 1 }
          , child =
              bin Frame [#label := "New Value", #hexpand := True, #margin := 2] $
                newValueEntry keyWindowKey keyWindowOldValue keyWindowNewValue keyWindowSave
          }
      ]

oldValueEntry
  :: EditableBytes
  -> Operation (Maybe EditableBytes)
  -> Widget (ComponentAction KeyWindow)
oldValueEntry key oldValue =
  container Box
    [ #orientation := OrientationVertical
    , #spacing := 4
    ] $
    [loadButton] <> loadedBoxChildren
  where
    loadButton = BoxChild
      { properties = defaultBoxChildProperties
      , child = widget Button
          [ #label := "Load value at key"
          , #halign := AlignStart
          , #sensitive := (oldValue /= OperationInProgress)
          , on #clicked (LoadKeyWindowOldValue key)
          ]
      }

    loadedBoxChildren = case oldValue of
      OperationNotStarted -> []
      OperationInProgress -> []
      OperationSuccess maybeValue ->
        let combo = existsCombo maybeValue Nothing False
            entry = case maybeValue of
              Nothing -> []
              Just mv ->
                [ BoxChild
                    { properties = defaultBoxChildProperties { expand = True, fill = True }
                    , child = component TupleEntry.tupleEntry
                        { TupleEntry.value = mv
                        }
                    }
                ]
        in [combo] <> entry
      OperationFailure msg ->
        [ BoxChild
            { properties = defaultBoxChildProperties { expand = True, fill = False }
            , child = widget Label [#label := msg]
            }
        ]

newValueEntry
  :: EditableBytes
  -> Operation (Maybe EditableBytes)
  -> Maybe EditableBytes
  -> Operation ()
  -> Widget (ComponentAction KeyWindow)
newValueEntry key oldValue newValue saveOperation =
  container Box
    [ #orientation := OrientationVertical
    , #spacing := 4
    ] $
    [copyButton, combo] <> entry <> [saveButton]
  where
    copyButton = BoxChild
      { properties = defaultBoxChildProperties
      , child = widget Button $
          [ #label := "Copy from existing value"
          , #halign := AlignStart
          ] <> case operationSuccess oldValue of
            Nothing -> [#sensitive := False]
            Just e  -> [on #clicked (UpdateKeyWindowNewValue e)]
      }

    combo =
      existsCombo newValue (Just (UpdateKeyWindowNewValue . onComboChange)) True
      
    entry = case newValue of
      Nothing -> []
      Just mv ->
        [ BoxChild
            { properties = defaultBoxChildProperties { expand = True, fill = True }
            , child = component TupleEntry.tupleEntry
                { TupleEntry.value = mv
                , TupleEntry.onChanged = Just (UpdateKeyWindowNewValue . Just)
                }
            }
        ]

    onComboChange pos =
      if pos == 0 then Just (Left "") else Nothing

    saveButton = BoxChild
      { properties = defaultBoxChildProperties {expand = True, fill = True}
      , child = container Box [#valign := AlignEnd]
          [ BoxChild
              { properties = defaultBoxChildProperties { expand = True, fill = True }
              , child = widget Label
                  [ #label := saveResultLabel
                  , #halign := AlignStart
                  ]
              }
          , BoxChild
              { properties = defaultBoxChildProperties
              , child = widget Button $
                  [ #label := "Save Value"
                  , #halign := AlignEnd
                  ] <> case saveOperation of
                    OperationInProgress -> [#sensitive := False]
                    _ -> [on #clicked (KeyWindowSave key newValue)]
              }
          ]
      }

    saveResultLabel = case saveOperation of
      OperationNotStarted  -> ""
      OperationInProgress  -> ""
      OperationFailure msg -> msg
      OperationSuccess ()  -> "Value saved successfully"

existsCombo
  :: Maybe EditableBytes
  -> Maybe (Int -> ComponentAction KeyWindow)
  -> Bool
  -> BoxChild (ComponentAction KeyWindow)
existsCombo maybeValue onChanged sensitive =
  BoxChild
    { properties = defaultBoxChildProperties
    , child = component ComboBoxText.comboBoxText
        { ComboBoxText.choices = ["Exists", "Doesn't Exist"]
        , ComboBoxText.position = Just (if isJust maybeValue then 0 else 1)
        , ComboBoxText.onChanged
        , ComboBoxText.rawAttributes = [#sensitive := sensitive]
        }
    }
