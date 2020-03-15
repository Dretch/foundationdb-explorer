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

import           Prelude                                     hiding (id)

import           Control.Concurrent                          (threadDelay)
import           Control.Exception                           (displayException)
import           Data.Either.Extra                           (mapLeft)
import           Data.Functor                                ((<&>))
import qualified Data.HashMap.Strict                         as HashMap
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
import           System.Random                               (randomIO)

import           FDBE.Event                                  (Event (..), KeyWindowEvent (..),
                                                              SearchEvent (..),
                                                              StatusEvent (..))
import           FDBE.FoundationDB                           (getKeyValue,
                                                              getSearchResult,
                                                              getStatus,
                                                              setKeyValue)
import qualified FDBE.KeyWindow                              as KeyWindow
import qualified FDBE.Search                                 as Search
import           FDBE.State                                  (KeyWindow (..),
                                                              Operation (..),
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
            , menuItem MenuItem [onM #activate (const (KeyWindowEvent . NewKeyWindow <$> randomIO))]
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
    keyWindowAttrs = Vector.fromList $
      HashMap.toList keyWindows <&> \(id, w) -> window id (KeyWindow.view' id w)

statusWindow :: Text -> Attribute w Event
statusWindow status = window () $ bin Window
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
        Transition state {search = search {searchResults = OperationInProgress}} $ do
          res <- getSearchResult database (searchRange search)
          pure . Just . SearchEvent . FinishSearch $ mapLeft (pack . displayException) res
      FinishSearch results ->
        let mkSuccess (searchDuration, searchSeq) =
              OperationSuccess SearchResults {searchViewFull = Nothing, ..}
            searchResults = either OperationFailure mkSuccess results
        in Transition state {search = search {searchResults}} (pure Nothing)
      SetSearchResultsViewFull viewFull ->
        case searchResults search of
          OperationSuccess SearchResults {..} ->
            Transition state {search =search { searchResults = OperationSuccess SearchResults {searchViewFull = viewFull, ..}}} (pure Nothing)
          _ ->
            Transition state (pure Nothing)

    updateKeyWindow = \case
      NewKeyWindow id ->
        updateKeyWindows (HashMap.insert id State.initialKeyWindow)
      UpdateKeyWindowKey id key ->
        updateKeyWindowAt id (\w -> Just w { keyWindowKey = key, keyWindowOldValue = OperationNotStarted })
      LoadKeyWindowOldValue id key ->
        updateKeyWindowAtM id (\w -> Just w { keyWindowOldValue = OperationInProgress }) $ do
          op <- getKeyValue database key >>= \case
            Left e    -> pure . OperationFailure . pack $ displayException e
            Right val -> pure $ OperationSuccess val
          pure . Just . KeyWindowEvent $ UpdateKeyWindowOldValue id op
      UpdateKeyWindowOldValue id op ->
        updateKeyWindowAt id (\w -> Just w { keyWindowOldValue = op })
      UpdateKeyWindowNewValue id val ->
        updateKeyWindowAt id (\w -> Just w { keyWindowNewValue = val, keyWindowSave = OperationNotStarted })
      KeyWindowSave id key value ->
        updateKeyWindowAtM id (\w -> Just w { keyWindowSave = OperationInProgress }) $ do
          op <- setKeyValue database key value >>= \case
            Just e  -> pure . OperationFailure . pack $ displayException e
            Nothing -> pure $ OperationSuccess ()
          pure . Just . KeyWindowEvent $ UpdateKeyWindowSave id op
      UpdateKeyWindowSave id op ->
        updateKeyWindowAt id (\w -> Just w { keyWindowSave = op })
      CloseKeyWindow id ->
        updateKeyWindowAt id (const Nothing)

    updateKeyWindows f =
      Transition state {keyWindows = f keyWindows} (pure Nothing)

    updateKeyWindowAt id f =
      updateKeyWindows (HashMap.update f id)

    updateKeyWindowAtM id f =
      Transition state {keyWindows = HashMap.update f id keyWindows}

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
