module FDBE.Event
  ( Event(..)
  , StatusEvent(..)
  , SearchEvent(..)
  ) where

import           Data.Sequence   (Seq)
import           Data.Text       (Text)
import           Data.Time.Clock (NominalDiffTime)
import           FDBE.State      (SearchRange, SearchResult,
                                  SearchResultsViewFull)

data Event
  = StatusEvent StatusEvent
  | SearchEvent SearchEvent
  | Close

data StatusEvent
  = ShowStatus
  | HideStatus
  | ReloadStatus
  | SetStatus Text

data SearchEvent
  = SetSearchRange SearchRange
  | StartSearch
  | FinishSearch (Either Text (NominalDiffTime, Seq SearchResult))
  | SetSearchResultsViewFull (Maybe SearchResultsViewFull)
