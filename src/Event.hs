module Event (Event (..)) where

import Data.Text (Text)

data Event = ClusterFileSelectionChanged (Maybe Text)
           | ClusterFileChosen Text
           | ReloadStatus
           | SetStatus Text
           | Close