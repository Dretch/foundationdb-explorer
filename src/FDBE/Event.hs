module FDBE.Event
  ( Event(..)
  , StatusEvent(..)
  , SearchEvent(..)
  , KeyWindowEvent(..)
  ) where

import           Data.Sequence   (Seq)
import           Data.Text       (Text)
import           Data.Time.Clock (NominalDiffTime)

import           FDBE.State      (EditableBytes, KeyWindowId, Operation (..),
                                  SearchRange, SearchResult,
                                  SearchResultsViewFull)

data Event
  = StatusEvent StatusEvent
  | SearchEvent SearchEvent
  | KeyWindowEvent KeyWindowEvent
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

data KeyWindowEvent
  = NewKeyWindow KeyWindowId
  | UpdateKeyWindowKey KeyWindowId EditableBytes
  | LoadKeyWindowOldValue KeyWindowId EditableBytes
  | UpdateKeyWindowOldValue KeyWindowId (Operation (Maybe EditableBytes))
  | UpdateKeyWindowNewValue KeyWindowId (Maybe EditableBytes)
  | KeyWindowSave KeyWindowId EditableBytes (Maybe EditableBytes)
  | UpdateKeyWindowSave KeyWindowId (Operation ())
  | CloseKeyWindow KeyWindowId
