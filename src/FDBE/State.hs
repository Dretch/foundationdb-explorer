{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module FDBE.State
  ( State(..)
  , Search(..)
  , SearchRange(..)
  , SearchResults(..)
  , SearchResult(..)
  , SearchResultsViewFull(..)
  , initialState
  , maxKeyTupleSize
  , maxValueTupleSize
  ) where

import           Data.ByteString          (ByteString)
import           Data.Sequence            (Seq, ViewR (..))
import qualified Data.Sequence            as S
import           Data.Text                (Text)
import           Data.Time.Clock          (NominalDiffTime, UTCTime)
import           FoundationDB             (Database)
import           FoundationDB.Layer.Tuple (Elem)

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
    { searchFrom  :: Text
    , searchTo    :: Text
    , searchLimit :: Integer
    }

data SearchResults
  = SearchNotStarted
  | SearchInProgress
  | SearchSuccess
      { searchResultsDuration :: NominalDiffTime
      , searchResultsSeq      :: Seq SearchResult
      , searchResultsViewFull :: Maybe SearchResultsViewFull
      }
  | SearchFailure Text
  deriving (Eq)

data SearchResultsViewFull =
  SearchResultsViewFull
    { viewFullText :: Text
    , viewFullTime :: UTCTime
    }
  deriving (Eq)

data SearchResult =
  SearchResult
    { resultKey   :: (ByteString, Maybe [Elem])
    , resultValue :: (ByteString, Maybe [Elem])
    }
  deriving (Eq, Show)

initialState :: Database -> State
initialState database =
  State
    { database
    , status = ""
    , search =
        Search
          { searchRange =
              SearchRange
                {searchFrom = "", searchTo = "\\xFF", searchLimit = 100}
          , searchResults = SearchNotStarted
          }
    }

maxKeyTupleSize :: Seq SearchResult -> Maybe Integer
maxKeyTupleSize = maxTupleSize . fmap resultKey

maxValueTupleSize :: Seq SearchResult -> Maybe Integer
maxValueTupleSize = maxTupleSize . fmap resultValue

maxTupleSize :: Seq (ByteString, Maybe [Elem]) -> Maybe Integer
maxTupleSize rows =
  case S.viewr $ S.sort $ fmap length . snd <$> rows of
    S.EmptyR -> Nothing
    _ :> a   -> fromIntegral <$> a
