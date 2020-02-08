{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module FDBE.App
  ( State
  , Event
  , app
  ) where

import           Control.Concurrent                    (threadDelay)
import           Control.Exception                     (displayException)
import           Data.Either.Extra                     (mapLeft)
import           Data.Text                             (pack)
import           FoundationDB                          (Database)
import           GI.Gtk                                (Align (..), Label (..),
                                                        Window (..), WindowPosition (..))
import           GI.Gtk.Declarative
import           GI.Gtk.Declarative.App.Simple
import           Pipes                                 (yield)
import           Pipes.Prelude                         (repeatM)

import           GI.Gtk.Declarative.Container.Notebook (notebook, page)

import           FDBE.Event                            (Event (..))
import           FDBE.FoundationDB                     (getSearchResult,
                                                        getStatus)
import qualified FDBE.Search                           as Search
import           FDBE.State                            (Search (..),
                                                        SearchResults (..),
                                                        State (..))
import qualified FDBE.State                            as State

view' :: State -> AppView Window Event
view' State {..} =
  bin
    Window
    [ #title := "FoundationDB Explorer"
    , on #deleteEvent $ const (True, Close)
    , #widthRequest := 500
    , #windowPosition := WindowPositionCenter
    ] $
  notebook
    []
    [ page "Search" $ Search.view' search
    , page "Status" $
      widget
        Label
        [ #label := status
        , classes ["status"]
        , #margin := 10
        , #halign := AlignStart
        , #valign := AlignStart
        ]
    ]

update' :: State -> Event -> Transition State Event
update' state@State {..} = \case
  ReloadStatus ->
    Transition state $ (Just . SetStatus) <$> getStatus database
  SetStatus status' ->
    Transition state {status = status'} (pure Nothing)
  SetSearchRange range ->
    Transition state {search = search {searchRange = range}} (pure Nothing)
  StartSearch ->
    Transition state {search = search {searchResults = SearchInProgress}} $ do
      res <- getSearchResult database (searchRange search)
      pure . Just . FinishSearch $ mapLeft (pack . displayException) res
  FinishSearch results ->
    let mkSuccess = \(searchResultsDuration, searchResultsSeq) ->
          SearchSuccess{searchResultsViewFull = Nothing, ..}
        searchResults = either SearchFailure mkSuccess results
     in Transition state {search = search {searchResults}} (pure Nothing)
  SetSearchResultsViewFull t ->
    Transition state {search = search { searchResults = (searchResults search) {searchResultsViewFull = t}}} (pure Nothing)
  Close ->
    Exit

app :: Database -> App Window State Event
app db =
  App
    { view = view'
    , update = update'
    , inputs = [reloadStatusPeriodically]
    , initialState = State.initialState db
    }
  where
    reloadStatusPeriodically = do
      yield ReloadStatus
      repeatM $ do
        threadDelay $ 5 * 1000 * 1000
        pure ReloadStatus
