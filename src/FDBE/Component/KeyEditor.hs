{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

module FDBE.Component.KeyEditor
( KeyEditorModel(..)
, initialModel
, keyEditor
) where

import FDBE.Prelude
import FDBE.State (Operation (..), operationSuccess)
import FDBE.FoundationDB (EditableBytes, decodeEditableBytes, getKeyValue, setKeyValue)
import Monomer
import Control.Lens hiding (op)
import Control.Exception (displayException)
import Data.Text (pack)
import FoundationDB (Database)
import FDBE.Component.TupleEntry (tupleEntry, tupleEntryV)
import FDBE.Monomer (titleLabel)
import Control.Monad (join)

-- todo: remove/shorten prefixes?
data KeyEditorModel = KeyEditorModel
  { _keyEditorKey      :: EditableBytes
  , _keyEditorOldValue :: Operation (Maybe EditableBytes)
  , _keyEditorNewValue :: Maybe EditableBytes
  , _keyEditorSave     :: Operation ()
  , _database          :: Database
  } deriving (Eq, Show)

makeLenses ''KeyEditorModel

data KeyEditorEvent
  = UpdateKeyEditorKey EditableBytes
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

keyEditor
 :: (CompositeEvent ep, CompParentModel sp)
 => KeyEditorModel
 -> (KeyEditorModel -> ep)
 -> WidgetNode sp ep
keyEditor model changeHandler =
  compositeV "FDBE.KeyEditor" model changeHandler buildUI handleEvent

buildUI :: UIBuilder KeyEditorModel KeyEditorEvent
buildUI _wenv model = stack where
  stack =
    vstack_ [childSpacing_ 10] [
      titleBox "Key" [tupleEntryV (model ^. keyEditorKey) UpdateKeyEditorKey],
      titleBox "Existing Value" (loadValueAtKeyButton : loadedBoxChildren),
      titleBox "New Value" [copyExistingButton]
    ]

  loadValueAtKeyButton =
    button "Load value at key" LoadKeyEditorOldValue
      `nodeEnabled` (model ^. keyEditorOldValue /= OperationInProgress)

  loadedBoxChildren = case model ^. keyEditorOldValue of
    OperationNotStarted -> []
    OperationInProgress -> []
    OperationSuccess maybeValue ->
      let existsVal = maybe ValueDoesntExist (const ValueExists) maybeValue
          combo = existsCombo existsVal (const PointlessEvent)
          entry = case maybeValue of
            Nothing -> []
            Just mv -> [tupleEntryV mv (UpdateKeyEditorNewValue . Just)]
      in (`nodeEnabled` False) <$> combo : entry
    OperationFailure msg ->
      [label msg]

  copyExistingButton =
    button "Copy from existing value" UpdateKeyEditorNewValueCopyOld
      `nodeEnabled` (isJust . operationSuccess $ model ^. keyEditorOldValue)

titleBox :: Text -> [WidgetNode KeyEditorModel KeyEditorEvent] -> WidgetNode KeyEditorModel KeyEditorEvent
titleBox title contents =
  -- todo:fix button borders! min width/height!
  box (vstack_ [childSpacing_ 2] (titleLabel title : contents))
  `styleBasic` [border 1 (rgbHex "#c9c6c2"), padding 4, radius 3]

existsCombo
  :: ValueExistsOrNot
  -> (ValueExistsOrNot -> KeyEditorEvent)
  -> WidgetNode KeyEditorModel KeyEditorEvent
existsCombo val handleChange =
  textDropdownSV val handleChange [ValueExists, ValueDoesntExist]

handleEvent :: EventHandler KeyEditorModel KeyEditorEvent sp ep
handleEvent _wenv _node model = \case
  UpdateKeyEditorKey key ->
    [Model (model & keyEditorKey .~ key & keyEditorOldValue .~ OperationNotStarted)]
  LoadKeyEditorOldValue ->
    [Model (model & keyEditorOldValue .~ OperationInProgress)
    ,Task $ do
      op <- getKeyValue (model ^. database) (model ^. keyEditorKey) >>= \case
        Left e    -> pure . OperationFailure . pack $ displayException e
        Right val -> pure $ OperationSuccess val
      pure $ UpdateKeyEditorOldValue op
    ]
  UpdateKeyEditorOldValue val ->
    [Model (model & keyEditorOldValue .~ val)]
  UpdateKeyEditorNewValue val ->
    [Model (model & keyEditorNewValue .~ val)]
  UpdateKeyEditorNewValueCopyOld ->
    let opSucc = join (operationSuccess (model ^. keyEditorOldValue))
     in [Model (model & keyEditorNewValue .~ opSucc)]
  KeyEditorSave ->
    [Model (model & keyEditorSave .~ OperationInProgress)
    ,Task $ do
      op <- setKeyValue (model ^. database) (model ^. keyEditorKey) (model ^. keyEditorNewValue) >>= \case
        Just e  -> pure . OperationFailure . pack $ displayException e
        Nothing -> pure $ OperationSuccess ()
      pure $ UpdateKeyEditorSave op
    ]
  UpdateKeyEditorSave val ->
    [Model (model & keyEditorSave .~ val)]
  PointlessEvent -> -- todo: avoid somehow...
    []

initialModel :: Database -> ByteString -> ByteString -> KeyEditorModel
initialModel database key value = KeyEditorModel
  { _keyEditorKey = keyEb
  , _keyEditorOldValue = OperationSuccess (Just valueEb)
  , _keyEditorNewValue = Nothing
  , _keyEditorSave = OperationNotStarted
  , _database = database
  }
  where
    keyEb = decodeEditableBytes key
    valueEb = decodeEditableBytes value