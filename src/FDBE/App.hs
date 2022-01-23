{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE LambdaCase #-}

module FDBE.App
  ( start,
  )
where

import FDBE.Prelude

import Control.Lens
import Monomer
import FoundationDB (Database)
import FDBE.Component.Search (search)
import FDBE.Component.Status (status)
import qualified FDBE.Font as Font
import FDBE.Monomer (adwaitaTheme, sizeReqUpdaterFlexMax)
import FDBE.Component.KeyEditor (KeyEditorModel)
import qualified FDBE.Component.KeyEditor as KeyEditor

data AppModel = AppModel
  { _database :: Database
  , _appAlert :: AppAlert 
  }
  deriving (Eq, Show)

data AppAlert
  = AlertNone
  | AlertStatus
  | AlertKeyEditor KeyEditorModel
  deriving (Eq, Show)

-- todo: why do these have to be together? https://stackoverflow.com/questions/47742054/haskell-makelenses-data-constructor-not-in-scope
makeLenses ''AppModel

data AppEvent
  = ShowKeyEditorAlert ByteString ByteString
  | ShowEmptyKeyEditorAlert
  | ShowStatusAlert
  | SetKeyEditorModel KeyEditorModel
  | HideAlert

buildUI :: UIBuilder AppModel AppEvent
buildUI _wenv model = widgetStack where

  widgetStack = zstack [
      vstack_ [sizeReqUpdaterFlexMax] [
        hgrid_ [childSpacing_ 2] [
          button "Database Status" ShowStatusAlert,
          button "Edit Value at Key" ShowEmptyKeyEditorAlert
        ] `styleBasic` [padding 2],
        search (model ^. database) ShowKeyEditorAlert
      ],
     maybeAlert
   ]
  
  -- todo: why do we get "("Failed match on Composite handleMessage",StatusEvent)" errors after changing from status to editor?
  maybeAlert = case model ^. appAlert of
    AlertNone ->
      spacer `nodeVisible` False
    AlertStatus ->
      alert HideAlert (status (model ^. database))
    AlertKeyEditor keModel ->
      alert HideAlert (KeyEditor.keyEditor keModel SetKeyEditorModel)

handleEvent :: EventHandler AppModel AppEvent sp ep
handleEvent _wenv _node model = \case
  ShowKeyEditorAlert key value ->
    [Model (model & appAlert .~ AlertKeyEditor (KeyEditor.initialModel_ (model ^. database) key value))]
  ShowEmptyKeyEditorAlert ->
    [Model (model & appAlert .~ AlertKeyEditor (KeyEditor.initialModel (model ^. database)))]
  ShowStatusAlert ->
    [Model (model & appAlert .~ AlertStatus)]
  HideAlert ->
    [Model (model & appAlert .~ AlertNone)]
  SetKeyEditorModel val ->
    [Model (model & appAlert .~ AlertKeyEditor val)]

start :: Database -> IO ()
start db = startApp model handleEvent buildUI config
  where
    config =
      [ appWindowTitle "FoundationDB Explorer"
      , appTheme adwaitaTheme
      ] <> Font.fontDefs
    model = AppModel
      { _database = db
      , _appAlert = AlertNone
      }
