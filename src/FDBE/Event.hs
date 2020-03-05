module FDBE.Event
  ( Event(..)
  , StatusEvent(..)
  , SearchEvent(..)
  , KeyWindowEvent(..)
  ) where

import           Data.Sequence   (Seq)
import           Data.Text       (Text)
import           Data.Time.Clock (NominalDiffTime)

import           FDBE.State      (EditableBytes, Operation (..), SearchRange,
                                  SearchResult, SearchResultsViewFull)

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
  = NewKeyWindow
  | UpdateKeyWindowKey Int EditableBytes
  | LoadKeyWindowOldValue Int EditableBytes
  | UpdateKeyWindowOldValue Int (Operation (Maybe EditableBytes))
  | UpdateKeyWindowNewValue Int (Maybe EditableBytes)
  | KeyWindowSave Int EditableBytes (Maybe EditableBytes)
  | UpdateKeyWindowSave Int (Operation ())
  | CloseKeyWindow Int
