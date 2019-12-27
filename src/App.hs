module App
  ( State
  , Event
  , app
  ) where

import Control.Concurrent (threadDelay)
import Data.Text (Text)
import GI.Gtk (Align (..), Label (..), Window (..))
import GI.Gtk.Declarative
import GI.Gtk.Declarative.App.Simple
import Pipes.Prelude (repeatM)

import qualified ClusterFileChooser
import qualified Search
import Event (Event (..))
import qualified State
import State (State (..), Search (..))
import FoundationDBUtil (getStatus, getSearchResult)
import Gi.Gtk.Declarative.Notebook (notebook, page)

view' :: State -> AppView Window Event
view' state =
    bin Window [#title := "FoundationDB Explorer", on #deleteEvent (const (True, Close)), #widthRequest := 500]
      $ case state of
          ChoosingClusterFile{..} ->
            ClusterFileChooser.view' selectedClusterFile
          ChosenClusterFile{..} ->
            notebook []
              [ page "Search" (Search.view' search)
              , page "Status" (widget Label [#label := status, #margin := 10, #halign := AlignStart, #valign := AlignStart])
              ]
            

update' :: State -> Event -> Transition State Event
update' state@ChoosingClusterFile{..} (ClusterFileSelectionChanged maybePath) =
    Transition state{selectedClusterFile = maybePath} (pure Nothing)

update' ChoosingClusterFile{..} (ClusterFileChosen clusterFilePath) =
    Transition (State.initialChosenClusterFileState clusterFilePath) (pure $ Just ReloadStatus)

update' state@ChosenClusterFile{..} ReloadStatus =
    Transition state $ (Just . SetStatus) <$> getStatus clusterFilePath

update' state@ChosenClusterFile{..} (SetStatus status') =
    Transition state{status = status'} (pure Nothing)

update' state@ChosenClusterFile{} (StartSearch range) =
    Transition state{search = (search state){searchInProgress = True, searchResults = Nothing}} $
        Just . FinishSearch <$> getSearchResult range

update' state@ChosenClusterFile{} (FinishSearch results) =
    Transition state{search = (search state){searchInProgress = False, searchResults = Just results}} (pure Nothing)

update' _state Close =
    Exit
    
update' state _ =
    Transition state (pure Nothing)

app :: Maybe Text -> App Window State Event
app defaultClusterFilePath = do
    App { view = view'
        , update = update'
        , inputs = [reloadStatusPeriodically]
        , initialState = State.initialState defaultClusterFilePath
        }
    where
        reloadStatusPeriodically = repeatM $ do
            threadDelay $ 5 * 1000 * 1000
            pure ReloadStatus
