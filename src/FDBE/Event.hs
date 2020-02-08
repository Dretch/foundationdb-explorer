module FDBE.Event
  ( Event(..)
  ) where

import           Data.Sequence   (Seq)
import           Data.Text       (Text)
import           Data.Time.Clock (NominalDiffTime)
import           FDBE.State      (SearchRange, SearchResult, SearchResultsViewFull)

data Event
  = ReloadStatus
  | SetStatus Text
  | SetSearchRange SearchRange
  | StartSearch
  | FinishSearch (Either Text (NominalDiffTime, Seq SearchResult))
  -- two events are needed instead of one, due to gi-gtk-declarative-app-simple API limitations
  | SetSearchResultsViewFullPre Text
  | SetSearchResultsViewFull (Maybe SearchResultsViewFull)
  | Close
