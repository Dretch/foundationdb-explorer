{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE OverloadedStrings          #-}

module FDBE.State
  ( Operation(..)
  , operationSuccess
  , EditableBytes
  , State(..)
  , Search(..)
  , SearchRange(..)
  , SearchResults(..)
  , SearchResult(..)
  , SearchResultsViewFull(..)
  , KeyWindowId(..)
  , KeyWindow(..)
  , initialState
  , initialKeyWindow
  , maxKeyTupleSize
  , maxValueTupleSize
  ) where

import           Data.ByteString          (ByteString)
import           Data.Hashable            (Hashable)
import           Data.HashMap.Strict      (HashMap)
import qualified Data.HashMap.Strict      as HashMap
import           Data.Sequence            (Seq, ViewR (..))
import qualified Data.Sequence            as S
import           Data.Text                (Text)
import           Data.Time.Clock          (NominalDiffTime, UTCTime)
import           Data.UUID                (UUID)
import           FoundationDB             (Database)
import           FoundationDB.Layer.Tuple (Elem)
import           System.Random            (Random)

data State =
  State
    { database      :: Database
    , status        :: Text
    , statusVisible :: Bool
    , search        :: Search
    , keyWindows    :: HashMap KeyWindowId KeyWindow
    }

-- | A bytestring (foundationdb key or value) can be edited either as raw bytes
-- (as ASCII text, with escape codes for binary), or as a structured tuple (if
-- the bytes can be decoded as a tuple)
type EditableBytes = Either Text [Elem]

-- | Some kind of operation that takes time, and that we want to update the UI
-- about before it has been completed. E.g. loading/saving database keys.
data Operation a
  = OperationNotStarted
  | OperationInProgress
  | OperationSuccess a
  | OperationFailure Text
  deriving (Eq)

data Search =
  Search
    { searchRange   :: SearchRange
    , searchResults :: Operation SearchResults
    }

data SearchRange =
  SearchRange
    { searchFrom    :: EditableBytes
    , searchTo      :: EditableBytes
    , searchLimit   :: Word
    , searchReverse :: Bool
    }

data SearchResults =
  SearchResults
    { searchDuration :: NominalDiffTime
    , searchSeq      :: Seq SearchResult
    , searchViewFull :: Maybe SearchResultsViewFull
    }
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

newtype KeyWindowId = KeyWindowId UUID
  deriving (Eq, Hashable, Random)

data KeyWindow =
  KeyWindow
    { keyWindowKey      :: EditableBytes
    , keyWindowOldValue :: Operation (Maybe EditableBytes)
    , keyWindowNewValue :: Maybe EditableBytes
    , keyWindowSave     :: Operation ()
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
          , searchResults = OperationNotStarted
          }
    , keyWindows = HashMap.empty
    }

initialKeyWindow :: KeyWindow
initialKeyWindow = KeyWindow
  { keyWindowKey = Left ""
  , keyWindowOldValue = OperationNotStarted
  , keyWindowNewValue = Nothing
  , keyWindowSave = OperationNotStarted
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

operationSuccess :: Operation a -> Maybe a
operationSuccess = \case
  OperationSuccess a -> Just a
  _ -> Nothing
