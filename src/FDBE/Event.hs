module FDBE.Event
  ( Event(..)
  ) where

import           Data.Sequence   (Seq)
import           Data.Text       (Text)
import           Data.Time.Clock (NominalDiffTime)
import           FDBE.State      (SearchRange, SearchResult, SearchResultsViewFull)

data Event
  = ShowStatus
  | HideStatus
  | ReloadStatus
  | SetStatus Text
  | SetSearchRange SearchRange
  | StartSearch
  | FinishSearch (Either Text (NominalDiffTime, Seq SearchResult))
  | SetSearchResultsViewFull (Maybe SearchResultsViewFull)
  | Close
