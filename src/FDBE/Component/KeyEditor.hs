{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

module FDBE.Component.KeyEditor
  ( KeyEditorModel (..),
    initialModel,
    initialModel_,
    keyEditor,
  )
where

import Control.Exception (displayException)
import Control.Lens hiding (op)
import Control.Monad (join)
import Data.Text (pack)
import FDBE.Component.TupleEntry (tupleEntryV, tupleEntryV_)
import qualified FDBE.Font as Font
import FDBE.FoundationDB (EditableBytes, decodeEditableBytes, getKeyValue, setKeyValue)
import FDBE.Monomer (sizeReqUpdaterFlexMax)
import FDBE.Prelude
import FDBE.State (Operation (..), operationSuccess)
import FoundationDB (Database)
import Monomer

data KeyEditorModel = KeyEditorModel
  { _keKey :: EditableBytes,
    _keOldValue :: Operation (Maybe EditableBytes),
    _keNewValue :: Maybe EditableBytes,
    _keSave :: Operation (),
    _keDatabase :: Database
  }
  deriving (Eq, Show)

makeLensesWith abbreviatedFields ''KeyEditorModel

data KeyEditorEvent
  = KeyEditorModelChanged KeyEditorModel
  | UpdateKeyEditorKey EditableBytes
  | LoadKeyEditorOldValue
  | UpdateKeyEditorOldValue (Operation (Maybe EditableBytes))
  | UpdateKeyEditorNewValue (Maybe EditableBytes)
  | UpdateKeyEditorNewValueCopyOld
  | KeyEditorSave
  | UpdateKeyEditorSave (Operation ())
  | PointlessEvent

data ValueExistsOrNot = ValueExists | ValueDoesntExist
  deriving (Eq)

instance Show ValueExistsOrNot where
  show ValueExists = "Exists"
  show ValueDoesntExist = "Doesn't Exist"

keyEditor ::
  (CompositeEvent ep, CompParentModel sp) =>
  KeyEditorModel ->
  (KeyEditorModel -> ep) ->
  WidgetNode sp ep
keyEditor model changeHandler =
  compositeV "FDBE.KeyEditor" model KeyEditorModelChanged buildUI (handleEvent changeHandler)

buildUI :: UIBuilder KeyEditorModel KeyEditorEvent
buildUI _wenv model =
  box_ [sizeReqUpdaterFlexMax] $
    scroll $
      vstack_
        [childSpacing_ 10]
        [ keyBox model,
          oldValueBox model,
          newValueBox model
        ]

keyBox :: KeyEditorModel -> WidgetNode KeyEditorModel KeyEditorEvent
keyBox model =
  titleBox "Key" [tupleEntryV (model ^. key) UpdateKeyEditorKey]

oldValueBox :: KeyEditorModel -> WidgetNode KeyEditorModel KeyEditorEvent
oldValueBox model = tree
  where
    tree = titleBox "Existing Value" (loadValueButton : loadedChildren)

    loadValueButton =
      button "Load value at key" LoadKeyEditorOldValue
        `nodeEnabled` (model ^. oldValue /= OperationInProgress)

    loadedChildren = case model ^. oldValue of
      OperationNotStarted -> []
      OperationInProgress -> []
      OperationSuccess maybeValue ->
        let existsVal = maybe ValueDoesntExist (const ValueExists) maybeValue
            combo = existsCombo existsVal (const PointlessEvent)
            entry = case maybeValue of
              Nothing -> []
              Just eb -> [tupleEntryV_ eb (UpdateKeyEditorNewValue . Just) True]
         in (combo `nodeEnabled` False) : entry
      OperationFailure msg ->
        [label msg]

newValueBox :: KeyEditorModel -> WidgetNode KeyEditorModel KeyEditorEvent
newValueBox model = tree
  where
    tree =
      titleBox
        "New Value"
        ([copyExistingButton, newValueTypeCombo] <> newValueEntry <> [saveButton])

    copyExistingButton =
      button "Copy from existing value" UpdateKeyEditorNewValueCopyOld
        `nodeEnabled` (isJust . operationSuccess $ model ^. oldValue)

    newValueTypeCombo = combo
      where
        combo = existsCombo existsVal (UpdateKeyEditorNewValue . onComboChange)
        existsVal
          | isJust (model ^. newValue) = ValueExists
          | otherwise = ValueDoesntExist
        onComboChange = \case
          ValueExists -> Just (Left "")
          ValueDoesntExist -> Nothing

    newValueEntry = case model ^. newValue of
      Nothing -> []
      Just eb -> [tupleEntryV eb (UpdateKeyEditorNewValue . Just)]

    saveButton =
      hstack
        [ label saveResultLabel,
          filler,
          button "Save" KeyEditorSave
            `nodeEnabled` (model ^. save /= OperationInProgress)
        ]

    saveResultLabel = case model ^. save of
      OperationNotStarted -> ""
      OperationInProgress -> ""
      OperationFailure msg -> msg
      OperationSuccess () -> "Saved successfully"

titleBox :: Text -> [WidgetNode KeyEditorModel KeyEditorEvent] -> WidgetNode KeyEditorModel KeyEditorEvent
titleBox titleText contents = tree
  where
    tree =
      box_ [alignLeft] (vstack_ [childSpacing_ 2] (title : contents))
        `styleBasic` [border 1 (rgbHex "#c9c6c2"), padding 4, radius 3]
    title =
      label titleText `styleBasic` [textFont Font.bold, paddingV 4]

existsCombo ::
  ValueExistsOrNot ->
  (ValueExistsOrNot -> KeyEditorEvent) ->
  WidgetNode KeyEditorModel KeyEditorEvent
existsCombo val handleChange =
  textDropdownSV val handleChange [ValueExists, ValueDoesntExist]

handleEvent :: (KeyEditorModel -> ep) -> EventHandler KeyEditorModel KeyEditorEvent sp ep
handleEvent changeHandler _wenv _node model = \case
  KeyEditorModelChanged newModel ->
    [Report (changeHandler newModel)]
  UpdateKeyEditorKey newKey ->
    [Model (model & key .~ newKey & oldValue .~ OperationNotStarted)]
  LoadKeyEditorOldValue ->
    [ Model (model & oldValue .~ OperationInProgress),
      Task $ do
        op <-
          getKeyValue (model ^. database) (model ^. key) >>= \case
            Left e -> pure . OperationFailure . pack $ displayException e
            Right val -> pure $ OperationSuccess val
        pure $ UpdateKeyEditorOldValue op
    ]
  UpdateKeyEditorOldValue val ->
    [Model (model & oldValue .~ val)]
  UpdateKeyEditorNewValue val ->
    [Model (model & newValue .~ val)]
  UpdateKeyEditorNewValueCopyOld ->
    let opSucc = join (operationSuccess (model ^. oldValue))
     in [Model (model & newValue .~ opSucc)]
  KeyEditorSave ->
    [ Model (model & save .~ OperationInProgress),
      Task $ do
        op <-
          setKeyValue (model ^. database) (model ^. key) (model ^. newValue) >>= \case
            Just e -> pure . OperationFailure . pack $ displayException e
            Nothing -> pure $ OperationSuccess ()
        pure $ UpdateKeyEditorSave op
    ]
  UpdateKeyEditorSave val ->
    [Model (model & save .~ val)]
  PointlessEvent ->
    -- todo: avoid somehow...
    []

initialModel :: Database -> KeyEditorModel
initialModel db =
  KeyEditorModel
    { _keKey = Left "",
      _keOldValue = OperationNotStarted,
      _keNewValue = Nothing,
      _keSave = OperationNotStarted,
      _keDatabase = db
    }

initialModel_ :: Database -> ByteString -> ByteString -> KeyEditorModel
initialModel_ db initKey initValue =
  KeyEditorModel
    { _keKey = keyEb,
      _keOldValue = OperationSuccess (Just valueEb),
      _keNewValue = Just valueEb,
      _keSave = OperationNotStarted,
      _keDatabase = db
    }
  where
    keyEb = decodeEditableBytes initKey
    valueEb = decodeEditableBytes initValue
