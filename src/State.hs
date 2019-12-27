module State
  ( State (..)
  , Search (..)
  , SearchRange (..)
  , SearchResult (..)
  , initialState
  , initialChosenClusterFileState
  ) where

import Data.Sequence (Seq)
import Data.Text (Text)

data State
    = ChoosingClusterFile{ selectedClusterFile :: Maybe Text }
    | ChosenClusterFile{ clusterFilePath :: Text, status :: Text, search :: Search }

data Search = Search
    { searchRange :: SearchRange
    , searchInProgress :: Bool
    , searchResults :: Maybe (Seq SearchResult)
    }

data SearchRange = SearchRange
    { searchFrom :: Text
    , searchTo :: Text
    }

data SearchResult = SearchResult
    { resultKey :: Text
    , resultValue :: Text
    }

initialState :: Maybe Text -> State
initialState selectedClusterFile =
    ChoosingClusterFile{selectedClusterFile}

initialChosenClusterFileState :: Text -> State
initialChosenClusterFileState clusterFilePath =
    ChosenClusterFile
      { clusterFilePath
      , status = ""
      , search = Search { searchRange = SearchRange { searchFrom = "", searchTo = "\\xFF" }, searchInProgress = False, searchResults = Nothing }
      }