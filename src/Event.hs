module Event (Event (..)) where

import Data.Sequence (Seq)
import Data.Text (Text)
import qualified State

data Event
    = ClusterFileSelectionChanged (Maybe Text)
    | ClusterFileChosen Text
    | ReloadStatus
    | SetStatus Text
    | StartSearch State.SearchRange
    | FinishSearch (Seq State.SearchResult)
    | Close