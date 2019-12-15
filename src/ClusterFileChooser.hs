module ClusterFileChooser (view') where

import Control.Monad (void)
import Data.Maybe (fromMaybe)
import Data.Text (Text, pack, unpack)
import GI.Gtk (Align (..), Box (..), Button (..), Label (..), Orientation (..), FileChooserButton (..), fileChooserGetFilename)
import GI.Gtk.Declarative

import Event (Event (..))

view' :: Maybe Text -> Widget Event
view' filePath =
  container Box [#orientation := OrientationVertical, #margin := 10, #spacing := 10]
    [ BoxChild
        defaultBoxChildProperties
        (widget Label [#label := "Select a Foundation DB cluster file", #halign := AlignStart])
    , BoxChild
        defaultBoxChildProperties
        (widget FileChooserButton [afterCreated setupFileChooserButton, onM #selectionChanged handleSelectionChanged])
    , BoxChild
        defaultBoxChildProperties
        (widget Button ([#label := "Select", #tooltipText := "Select the chosen file", #halign := AlignEnd] <> case filePath of
            Just p -> [on #clicked $ ClusterFileChosen p]
            Nothing -> []))
    ]
  where
    setupFileChooserButton :: FileChooserButton -> IO ()
    setupFileChooserButton button = do
        void $ #selectFilename button $ unpack $ fromMaybe "" filePath
    
    handleSelectionChanged :: FileChooserButton -> IO Event
    handleSelectionChanged b =
        ClusterFileSelectionChanged <$> fmap pack <$> fileChooserGetFilename b
