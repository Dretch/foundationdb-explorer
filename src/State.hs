{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module State
  ( State(..)
  , Search(..)
  , SearchRange(..)
  , SearchResults(..)
  , SearchResult(..)
  , initialState
  , maxKeyTupleSize
  , maxValueTupleSize
  ) where

import           Data.Sequence (Seq, ViewR (..))
import qualified Data.Sequence as S
import           Data.Text     (Text)
import           FoundationDB  (Database)

data State =
  State
    { database :: Database
    , status   :: Text
    , search   :: Search
    }

data Search =
  Search
    { searchRange   :: SearchRange
    , searchResults :: SearchResults
    }

data SearchRange =
  SearchRange
    { searchFrom :: Text
    , searchTo   :: Text
    }

data SearchResults
  = SearchNotStarted
  | SearchInProgress
  | SearchSuccess (Seq SearchResult)
  | SearchFailure Text
  deriving (Eq)

data SearchResult =
  SearchResult
    { resultKey   :: (Text, Maybe [Text])
    , resultValue :: (Text, Maybe [Text])
    }
  deriving (Eq, Show)

initialState :: Database -> State
initialState database =
  State
    { database
    , status = ""
    , search =
        Search
          { searchRange = SearchRange {searchFrom = "", searchTo = "\\xFF"}
          , searchResults = SearchNotStarted
          }
    }

maxKeyTupleSize :: Seq SearchResult -> Maybe Integer
maxKeyTupleSize = maxTupleSize resultKey

maxValueTupleSize :: Seq SearchResult -> Maybe Integer
maxValueTupleSize = maxTupleSize resultValue

maxTupleSize ::
     (SearchResult -> (Text, Maybe [Text])) -> Seq SearchResult -> Maybe Integer
maxTupleSize getter rows =
  case S.viewr $ S.sort $ fmap length . snd . getter <$> rows of
    S.EmptyR -> Nothing
    _ :> a   -> fromIntegral <$> a
