{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module FDBE.State
  ( EditableBytes
  , State(..)
  , Search(..)
  , SearchRange(..)
  , SearchResults(..)
  , SearchResult(..)
  , SearchResultsViewFull(..)
  , KeyWindow(..)
  , initialState
  , initialKeyWindow
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
    { database      :: Database
    , status        :: Text
    , statusVisible :: Bool
    , search        :: Search
    , keyWindows    :: [KeyWindow]
    }

-- | A bytestring (foundationdb key or value) can be edited either as raw bytes
-- (as ASCII text, with escape codes for binary), or as a structured tuple (if
-- the bytes can be decoded as a tuple)
type EditableBytes = Either Text [Elem]

data Search =
  Search
    { searchRange   :: SearchRange
    , searchResults :: SearchResults
    }

data SearchRange =
  SearchRange
    { searchFrom    :: EditableBytes
    , searchTo      :: EditableBytes
    , searchLimit   :: Word
    , searchReverse :: Bool
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
  deriving (Eq)

data KeyWindow =
  KeyWindow
    { keyWindowKey             :: EditableBytes
    , keyWindowOldValue        :: Maybe (Maybe EditableBytes)
    , keyWindowOldValueLoading :: Bool
    , keyWindowNewValue        :: Maybe EditableBytes
    , keyWindowSaving          :: Bool
    }

initialState :: Database -> State
initialState database =
  State
    { database
    , status = ""
    , statusVisible = False
    , search =
        Search
          { searchRange =
              SearchRange
                { searchFrom = Left ""
                , searchTo = Left "\\xFF"
                , searchLimit = 100
                , searchReverse = False
                }
          , searchResults = SearchNotStarted
          }
    , keyWindows = []
    }

initialKeyWindow :: KeyWindow
initialKeyWindow = KeyWindow
  { keyWindowKey = Left ""
  , keyWindowOldValue = Nothing
  , keyWindowOldValueLoading = False
  , keyWindowNewValue = Nothing
  , keyWindowSaving = False
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
