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
import FDBE.Monomer (compactTheme)

data AppModel = AppModel
  { _database :: Database
  , _visibleSection :: AppSection
  }
  deriving (Show)

-- todo: something less awkward / error prone (write an issue?)
instance Eq AppModel where
  a == b =
    _visibleSection a == _visibleSection b

data AppSection
  = SplashSection
  | StatusSection
  | SearchSection
  deriving (Eq, Show)

-- todo: why do these have to be together? https://stackoverflow.com/questions/47742054/haskell-makelenses-data-constructor-not-in-scope
makeLenses ''AppModel

data AppEvent
  = OpenSection AppSection

buildUI ::
  WidgetEnv AppModel AppEvent ->
  AppModel ->
  WidgetNode AppModel AppEvent
buildUI _wenv model = tree where

  tree = case model ^. visibleSection of
    StatusSection ->
      status (model ^. database)
    SplashSection ->
      hstack [
        filler,
        vstack [
          filler,
          label "FoundationDB Explorer" `styleBasic` [textSize 40, textCenter, padding 15],
          label "What would you like to do?" `styleBasic` [textSize 16, textCenter, padding 15],
          spacer,
          bigButton "View Database Status" StatusSection,
          spacer,
          bigButton "Fetch Keys and Values" SearchSection,
          spacer,
          bigButton "Write or Update a Value" SplashSection,
          spacer,
          image_ "./assets/icon.png" [alignCenter] `styleBasic` [padding 20, maxHeight 100, maxWidth 100],
          filler
        ],
        filler
      ]
    SearchSection ->
      search (model ^. database)
  
  bigButton msg stn =
    button msg (OpenSection stn) `styleBasic` [border 4 lightGray, radius 10, textSize 20]

handleEvent ::
  WidgetEnv AppModel AppEvent ->
  WidgetNode AppModel AppEvent ->
  AppModel ->
  AppEvent ->
  [AppEventResponse AppModel AppEvent]
handleEvent _wenv _node model = \case
  OpenSection s ->
    [Model (model & visibleSection .~ s)]

theme :: Theme
theme = compactTheme darkTheme

start :: Database -> IO ()
start db = startApp model handleEvent buildUI config
  where
    config =
      [ appWindowTitle "FoundationDB Explorer"
      , appTheme theme
      ] <> Font.fontDefs
    model = AppModel
      { _database = db
      , _visibleSection = SplashSection
      }
