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

import           Control.Concurrent                          (threadDelay)
import           Control.Exception                           (displayException)
import           Data.Either.Extra                           (mapLeft)
import           Data.List.Index                             (updateAt)
import           Data.Text                                   (Text, pack)
import qualified Data.Vector                                 as Vector
import           FoundationDB                                (Database)
import           GI.Gtk                                      (Align (..),
                                                              Box (..),
                                                              Label (..),
                                                              MenuBar (..),
                                                              MenuItem (..),
                                                              Orientation (..),
                                                              Window (..),
                                                              WindowPosition (..))
import           GI.Gtk.Declarative
import           GI.Gtk.Declarative.App.Simple
import           GI.Gtk.Declarative.Attributes.Custom.Window (window)
import           Pipes                                       (yield)
import           Pipes.Prelude                               (repeatM)

import           FDBE.Event                                  (Event (..), KeyWindowEvent (..),
                                                              SearchEvent (..),
                                                              StatusEvent (..))
import           FDBE.FoundationDB                           (getKeyValue,
                                                              getSearchResult,
                                                              getStatus)
import qualified FDBE.KeyWindow                              as KeyWindow
import qualified FDBE.Search                                 as Search
import           FDBE.State                                  (KeyWindow (..),
                                                              Search (..),
                                                              SearchResults (..),
                                                              State (..))
import qualified FDBE.State                                  as State

view' :: State -> AppView Window Event
view' State {..} =
  bin
    Window
    ([ #title := "FoundationDB Explorer"
    , on #deleteEvent $ const (True, Close)
    , #widthRequest := 800
    , #heightRequest := 800
    , #windowPosition := WindowPositionCenter
    ] <> statusWindowAttr <> keyWindowAttrs) $
  container
    Box
    [#orientation := OrientationVertical]
    [ container MenuBar []
        [ subMenu
            "Foundation DB"
            [ menuItem MenuItem [on #activate (StatusEvent ShowStatus)]
                $ widget Label [#label := "Database Status", #halign := AlignStart]
            , menuItem MenuItem [on #activate (KeyWindowEvent NewKeyWindow)]
                $ widget Label [#label := "Edit Value at Key", #halign := AlignStart]
            ]
        ]
    , BoxChild
        { properties = defaultBoxChildProperties { fill = True, expand = True }
        , child = Search.view' search
        }
    ]
  where
    statusWindowAttr
      | statusVisible = [statusWindow status]
      | otherwise     = []
    keyWindowAttrs =
      Vector.imap
        (\i w -> window $ KeyWindow.view' i w)
        (Vector.fromList keyWindows)

statusWindow :: Text -> Attribute w Event
statusWindow status = window $ bin Window
  [ #title := "Database Status"
  , on #deleteEvent (const (True, StatusEvent HideStatus))
  , #windowPosition := WindowPositionCenter
  ] $
  widget
    Label
    [ #label := status
    , classes ["status"]
    , #margin := 10
    , #halign := AlignStart
    , #valign := AlignStart
    ]

update' :: State -> Event -> Transition State Event
update' state@State {..} = \case
  StatusEvent e    -> updateStatus e
  SearchEvent e    -> updateSearch e
  KeyWindowEvent e -> updateKeyWindow e
  Close         -> Exit
  where
    updateStatus = \case
      ShowStatus ->
        Transition state { statusVisible = True } (pure Nothing)
      HideStatus ->
        Transition state { statusVisible = False } (pure Nothing)
      ReloadStatus ->
        Transition state $ Just . StatusEvent . SetStatus <$> getStatus database
      SetStatus status' ->
        Transition state {status = status'} (pure Nothing)

    updateSearch = \case
      SetSearchRange range ->
        Transition state {search = search {searchRange = range}} (pure Nothing)
      StartSearch ->
        Transition state {search = search {searchResults = SearchInProgress}} $ do
          res <- getSearchResult database (searchRange search)
          pure . Just . SearchEvent . FinishSearch $ mapLeft (pack . displayException) res
      FinishSearch results ->
        let mkSuccess (searchResultsDuration, searchResultsSeq) =
              SearchSuccess{searchResultsViewFull = Nothing, ..}
            searchResults = either SearchFailure mkSuccess results
        in Transition state {search = search {searchResults}} (pure Nothing)
      SetSearchResultsViewFull viewFull ->
        case searchResults search of
          SearchSuccess {..} ->
            Transition state {search = search { searchResults = SearchSuccess {searchResultsViewFull = viewFull, ..}}} (pure Nothing)
          _ ->
            Transition state (pure Nothing)

    updateKeyWindow = \case
      NewKeyWindow ->
        updateKeyWindows (State.initialKeyWindow :)
      UpdateKeyWindowKey i key ->
        updateKeyWindowAt i (\w -> Just w { keyWindowKey = key, keyWindowOldValue = Nothing })
      LoadWindowKeyOldValue i key ->
        updateKeyWindowAtM i (\w -> Just w { keyWindowOldValue = Nothing, keyWindowOldValueLoading = True }) $
          getKeyValue database key >>= \case
            Left _err ->
              pure Nothing
            Right val ->
              pure . Just $ KeyWindowEvent . UpdateKeyWindowOldValue i $ Just val
      UpdateKeyWindowOldValue i val ->
        updateKeyWindowAt i (\w -> Just w { keyWindowOldValue = val, keyWindowOldValueLoading = False })
      UpdateKeyWindowNewValue i val ->
        updateKeyWindowAt i (\w -> Just w { keyWindowNewValue = val })
      KeyWindowSave i key value ->
        updateKeyWindowAt i (\w -> Just w { keyWindowSaving = True })
      CloseKeyWindow i ->
        updateKeyWindowAt i (const Nothing)

    updateKeyWindows f =
      Transition state {keyWindows = f keyWindows} (pure Nothing)

    updateKeyWindowAt i f =
      updateKeyWindows (updateAt i f)

    updateKeyWindowAtM i f =
      Transition state {keyWindows = updateAt i f keyWindows}

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
      yield $ StatusEvent ReloadStatus
      repeatM $ do
        threadDelay $ 5 * 1000 * 1000
        pure $ StatusEvent ReloadStatus
