module Splash
    ( State(..)
    , Event
    , app
    ) where

import Control.Monad (void)
import Data.Maybe (fromMaybe)
import Data.Text (Text, pack, unpack)
import GI.Gtk (Align (..), Box (..), Button (..), FileFilter, Label (..), Window (..), Orientation (..), FileChooserButton (..), fileChooserGetFilename)
import GI.Gtk.Declarative
import GI.Gtk.Declarative.App.Simple

data State = State { filePath :: Maybe Text }

data Event = FileSelectionChanged (Maybe FilePath) | Close | Finish

view' :: State -> AppView Window Event
view' State{filePath} =
  bin Window [#title := "FoundationDB Explorer", on #deleteEvent (const (True, Close)), #widthRequest := 600]
    $ container Box [#orientation := OrientationVertical, #margin := 10, #spacing := 10]
        [ BoxChild
            defaultBoxChildProperties
            (widget Label [#label := "Select a Foundation DB cluster file", #halign := AlignStart])
        , BoxChild
            defaultBoxChildProperties
            (widget FileChooserButton [afterCreated setupFileChooserButton, onM #selectionChanged (fmap FileSelectionChanged . fileChooserGetFilename)])
        , BoxChild
            defaultBoxChildProperties
            (widget Button [#label := "Select", #tooltipText := "Select the chosen file", #halign := AlignEnd, on #clicked Finish])
        ]
  where
    setupFileChooserButton :: FileChooserButton -> IO ()
    setupFileChooserButton button = do
        void $ #selectFilename button $ unpack $ fromMaybe "" filePath

update' :: State -> Event -> Transition State Event
update' state = \case
    FileSelectionChanged maybePath ->
        Transition state{ filePath = pack <$> maybePath} (pure Nothing)
    Close ->
        Transition state{ filePath = Nothing } (pure $ Just Finish)
    Finish ->
        Exit

app :: Maybe Text -> App Window State Event
app defaultClusterFilePath = do
    App { view = view'
        , update = update'
        , inputs = []
        , initialState = State defaultClusterFilePath
        }
