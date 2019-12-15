module MainWindow
  ( State
  , Event
  , app
  ) where

import Control.Concurrent (threadDelay)
import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import Data.Maybe (fromMaybe)
import Data.Text (Text, pack, unpack)
import GI.Gtk (Align (..), Box (..), Button (..), FileFilter, Label (..), Window (..), Orientation (..), FileChooserButton (..), fileChooserGetFilename, textViewSetMonospace)
import GI.Gtk.Declarative
import GI.Gtk.Declarative.App.Simple
import Pipes (yield)
import Pipes.Prelude (repeatM)

import FoundationDBUtil (getStatus)

data State = State { clusterFilePath :: Text, status :: Text }

data Event = ReloadStatus | SetStatus Text | Close

view' :: State -> AppView Window Event
view' State{..} =
    bin Window [#title := "FoundationDB Explorer", on #deleteEvent (const (True, Close)), #widthRequest := 500, #heightRequest := 500]
      $ widget Label [#label := status, #margin := 10]

update' :: State -> Event -> Transition State Event
update' state@State{..} = \case
    ReloadStatus -> do
        Transition state $ (Just . SetStatus) <$> getStatus clusterFilePath
    SetStatus status ->
        Transition state{status} (pure Nothing)
    Close ->
        Exit

app :: Text -> App Window State Event
app clusterFilePath = do
    App { view = view'
        , update = update'
        , inputs = [reloadStatusPeriodically]
        , initialState = State clusterFilePath ""
        }
    where
        reloadStatusPeriodically = do
            yield ReloadStatus
            repeatM $ do
                threadDelay $ 5 * 1000 * 1000
                pure ReloadStatus