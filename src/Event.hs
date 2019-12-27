module Event (Event (..)) where

import Data.Sequence (Seq)
import Data.Text (Text)
import qualified State

data Event
    = ReloadStatus
    | SetStatus Text
    | StartSearch State.SearchRange
    | FinishSearch (Either Text (Seq State.SearchResult))
    | Close