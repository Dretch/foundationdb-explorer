module State
  ( State (..)
  , Search (..)
  , SearchRange (..)
  , SearchResults (..)
  , SearchResult (..)
  , initialState
  ) where

import Data.Sequence (Seq)
import Data.Text (Text)
import FoundationDB (Database)

data State
    = State{ database :: Database, status :: Text, search :: Search }

data Search = Search
    { searchRange :: SearchRange
    , searchResults :: SearchResults
    }

data SearchRange = SearchRange
    { searchFrom :: Text
    , searchTo :: Text
    }

data SearchResults
    = SearchNotStarted
    | SearchInProgress
    | SearchSuccess (Seq SearchResult)
    | SearchFailure Text
    deriving (Eq)

data SearchResult = SearchResult
    { resultKey :: Text
    , resultValue :: Text
    } deriving (Eq, Show)

initialState :: Database -> State
initialState database = State
    { database
    , status = ""
    , search = Search { searchRange = SearchRange { searchFrom = "", searchTo = "\\xFF" }, searchResults = SearchNotStarted }
    }