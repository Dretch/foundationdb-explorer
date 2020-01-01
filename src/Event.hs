module Event
  ( Event(..)
  ) where

import           Data.Sequence   (Seq)
import           Data.Text       (Text)
import           Data.Time.Clock (NominalDiffTime)
import           State           (SearchRange, SearchResult)

data Event
  = ReloadStatus
  | SetStatus Text
  | SetSearchRange SearchRange
  | StartSearch
  | FinishSearch (Either Text (NominalDiffTime, Seq SearchResult))
  | Close
