module App
  ( State
  , Event
  , app
  ) where

import Control.Concurrent (threadDelay)
import Data.Text (Text)
import GI.Gtk (Label (..), Window (..))
import GI.Gtk.Declarative
import GI.Gtk.Declarative.App.Simple
import Pipes.Prelude (repeatM)

import qualified ClusterFileChooser
import Event (Event (..))
import State (State (..))
import FoundationDBUtil (getStatus)

view' :: State -> AppView Window Event
view' state =
    bin Window [#title := "FoundationDB Explorer", on #deleteEvent (const (True, Close)), #widthRequest := 500, #heightRequest := 500]
      $ case state of
          ChoosingClusterFile{..} ->
            ClusterFileChooser.view' selectedClusterFile
          ChosenClusterFile{..} ->
            widget Label [#label := status, #margin := 10]

update' :: State -> Event -> Transition State Event
update' state@(ChoosingClusterFile _) (ClusterFileSelectionChanged maybePath) =
    Transition state{selectedClusterFile = maybePath} (pure Nothing)

update' (ChoosingClusterFile _) (ClusterFileChosen clusterFilePath) =
    Transition ChosenClusterFile{clusterFilePath, status = ""} (pure $ Just ReloadStatus)

update' state@ChosenClusterFile{..} ReloadStatus =
    Transition state $ (Just . SetStatus) <$> getStatus clusterFilePath

update' state@ChosenClusterFile{..} (SetStatus status') =
    Transition state{status = status'} (pure Nothing)
    
update' _state Close =
    Exit
    
update' state _ =
    Transition state (pure Nothing)

app :: Maybe Text -> App Window State Event
app defaultClusterFilePath = do
    App { view = view'
        , update = update'
        , inputs = [reloadStatusPeriodically]
        , initialState = ChoosingClusterFile{selectedClusterFile = defaultClusterFilePath}
        }
    where
        reloadStatusPeriodically = repeatM $ do
            threadDelay $ 5 * 1000 * 1000
            pure ReloadStatus
