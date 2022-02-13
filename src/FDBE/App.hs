{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TemplateHaskell #-}

module FDBE.App
  ( start,
  )
where

import Control.Lens
import FDBE.Component.KeyEditor (KeyEditorModel)
import qualified FDBE.Component.KeyEditor as KeyEditor
import FDBE.Component.Search (search)
import FDBE.Component.Status (status)
import qualified FDBE.Font as Font
import FDBE.Monomer (adwaitaTheme, sizeReqUpdaterFlexMax)
import FDBE.Prelude
import FoundationDB (Database)
import Monomer

data AppModel = AppModel
  { _database :: Database,
    _appAlert :: AppAlert
  }
  deriving (Eq, Show)

data AppAlert
  = AlertNone
  | AlertStatus
  | AlertKeyEditor KeyEditorModel
  deriving (Eq, Show)

makeLenses ''AppModel

data AppEvent
  = ShowKeyEditorAlert ByteString ByteString
  | ShowEmptyKeyEditorAlert
  | ShowStatusAlert
  | SetKeyEditorModel KeyEditorModel
  | HideAlert

buildUI :: UIBuilder AppModel AppEvent
buildUI _wenv model = widgetStack
  where
    widgetStack =
      zstack
        [ vstack_
            [sizeReqUpdaterFlexMax]
            [ search (model ^. database) ShowKeyEditorAlert,
              hgrid_
                [childSpacing_ 2]
                [ menuButton "Database Status" ShowStatusAlert,
                  menuButton "Edit Value at Key" ShowEmptyKeyEditorAlert
                ]
                `styleBasic` [padding 2]
            ]
            `nodeKey` "vstack", -- key needed to avoid recreating this vstack when opening the alert
          maybeAlert
        ]

    menuButton text event =
      button text event `styleBasic` [border 0 def]

    -- todo: why do we get "("Failed match on Composite handleMessage",StatusEvent)" errors after changing from status to editor?
    maybeAlert = case model ^. appAlert of
      AlertNone ->
        spacer `nodeVisible` False
      AlertStatus ->
        alert_ HideAlert [titleCaption "Database Status"] (status (model ^. database))
      AlertKeyEditor keModel ->
        alert_ HideAlert [titleCaption "Edit Value at Key"] (KeyEditor.keyEditor keModel SetKeyEditorModel)

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
      [ appWindowTitle "FoundationDB Explorer",
        appWindowIcon "./assets/icon.bmp",
        appTheme adwaitaTheme
      ]
        <> Font.fontDefs
    model =
      AppModel
        { _database = db,
          _appAlert = AlertNone
        }
