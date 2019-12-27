module Event (Event (..)) where

import Data.Sequence (Seq)
import Data.Text (Text)
import State (SearchRange, SearchResult)

data Event
    = ReloadStatus
    | SetStatus Text
    | SetSearchRange SearchRange
    | StartSearch
    | FinishSearch (Either Text (Seq SearchResult))
    | Close