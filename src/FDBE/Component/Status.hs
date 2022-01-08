{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE LambdaCase #-}

module FDBE.Component.Status (status) where

import FDBE.Prelude
import Control.Concurrent                          (threadDelay)
import FDBE.FoundationDB (getStatus)

import Control.Lens
import Monomer
import FoundationDB (Database)

data StatusModel = StatusModel
  { _database :: Database
  , _statusText :: Text
  }
  deriving (Eq, Show)

makeLenses ''StatusModel

data StatusEvent
 = WaitThenLoadStatus Int
 | SetStatus Text

buildUI :: UIBuilder StatusModel StatusEvent
buildUI _wenv model =
  scroll $
    label_ (model ^. statusText) [multiline]
     `styleBasic` [padding 4, textFont "Mono"]

handleEvent :: EventHandler StatusModel StatusEvent sp ep
handleEvent _wenv _node model = \case
  WaitThenLoadStatus seconds ->
   [Producer $ \handler -> do
      threadDelay $ seconds * 1000 * 1000
      getStatus (model ^. database) >>= handler . SetStatus
      handler $ WaitThenLoadStatus 5
    ]
  SetStatus s ->
    [Model (model & statusText .~ s)]

status :: (Typeable s, Typeable e) => Database -> WidgetNode s e
status db = compositeD_ "FBBE.Status" (WidgetValue initialModel) buildUI handleEvent [onInit initialEvent] where
  initialModel = StatusModel
    { _database = db
    , _statusText = "..."
    }
  initialEvent = WaitThenLoadStatus 0