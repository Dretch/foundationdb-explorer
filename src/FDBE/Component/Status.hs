{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TemplateHaskell #-}

module FDBE.Component.Status (status) where

import Control.Concurrent (threadDelay)
import Control.Lens
import FDBE.FoundationDB (getStatus)
import FDBE.Monomer (sizeReqUpdaterFlexMax, useOldCompositeModel)
import FDBE.Prelude
import FoundationDB (Database)
import Monomer

data StatusModel = StatusModel
  { _database :: Database,
    _statusText :: Text
  }
  deriving (Eq, Show)

makeLenses ''StatusModel

data StatusEvent
  = WaitThenLoadStatus Int
  | SetStatus Text

buildUI :: UIBuilder StatusModel StatusEvent
buildUI _wenv model = tree
  where
    tree =
      box_ [sizeReqUpdaterFlexMax] $
        scroll $
          label_ (model ^. statusText) [multiline]
            `styleBasic` [padding 4, textFont "Mono"]

handleEvent :: EventHandler StatusModel StatusEvent sp ep
handleEvent _wenv _node model = \case
  WaitThenLoadStatus seconds ->
    [ Producer $ \handler -> do
        threadDelay $ seconds * 1000 * 1000
        getStatus (model ^. database) >>= handler . SetStatus
        handler $ WaitThenLoadStatus 5
    ]
  SetStatus s ->
    [Model (model & statusText .~ s)]

status :: (Typeable s, Typeable e) => Database -> WidgetNode s e
status db = compositeD_ "FBBE.Status" (WidgetValue initialModel) buildUI handleEvent cfg
  where
    initialModel =
      StatusModel
        { _database = db,
          _statusText = "..."
        }
    cfg = [onInit (WaitThenLoadStatus 0), useOldCompositeModel]
