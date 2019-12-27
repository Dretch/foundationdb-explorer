module App
  ( State
  , Event
  , app
  ) where

import Control.Concurrent (threadDelay)
import Control.Exception (displayException)
import Data.Text (pack)
import FoundationDB (Database)
import GI.Gtk (Align (..), Label (..), Window (..))
import GI.Gtk.Declarative
import GI.Gtk.Declarative.App.Simple
import Pipes (yield)
import Pipes.Prelude (repeatM)

import qualified Search
import Event (Event (..))
import qualified State
import State (State (..), Search (..), SearchResults (..))
import FoundationDBUtil (getStatus, getSearchResult)
import Gi.Gtk.Declarative.Notebook (notebook, page)

view' :: State -> AppView Window Event
view' State{..} =
    bin Window [#title := "FoundationDB Explorer", on #deleteEvent (const (True, Close)), #widthRequest := 500]
      $ notebook []
          [ page "Search" (Search.view' search)
          , page "Status" (widget Label [#label := status, #margin := 10, #halign := AlignStart, #valign := AlignStart])
          ] 

update' :: State -> Event -> Transition State Event
update' state@State{..} ReloadStatus =
    Transition state $ (Just . SetStatus) <$> getStatus database

update' state@State{..} (SetStatus status') =
    Transition state{status = status'} (pure Nothing)

update' state@State{search} (SetSearchRange range) =
    Transition state{search = search{searchRange = range}} (pure Nothing)

update' state@State{database, search} StartSearch =
    Transition state{search = search{searchResults = SearchInProgress}} $ do
        res <- getSearchResult database (searchRange search)
        pure . Just . FinishSearch $ either (Left . pack . displayException) Right res

update' state@State{} (FinishSearch results) =
    let searchResults = either SearchFailure SearchSuccess results in
    Transition state{search = (search state){searchResults}} (pure Nothing)

update' _state Close =
    Exit

app :: Database -> App Window State Event
app db = do
    App { view = view'
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
