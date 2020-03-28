{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE OverloadedStrings          #-}

module FDBE.State
  ( Operation(..)
  , operationSuccess
  , EditableBytes
  , EditableElem(..)
  , fromEditableElem
  , toEditableElem
  , elemText
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

import           FDBE.Prelude

import qualified Data.HashMap.Strict       as HashMap
import qualified Data.Sequence             as S
import qualified Data.Text                 as T
import           Data.Time.Clock           (NominalDiffTime, UTCTime)
import           FoundationDB              (Database)
import           FoundationDB.Layer.Tuple  (Elem)
import qualified FoundationDB.Layer.Tuple  as LT
import qualified FoundationDB.Versionstamp as LV
import           System.Random             (Random)

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
type EditableBytes = Either Text [EditableElem]

-- | Like a FoundationDB Tuple Elem, but distinguishes between single and multi-line
-- text so they can be edited with different types of text entry widget.
data EditableElem
  = None
  | Tuple [EditableElem]
  | Bytes ByteString
  | SingleLineText Text
  | MultiLineText Text
  | Int Integer
  | Float Float
  | Double Double
  | Bool Bool
  | UUID Word32 Word32 Word32 Word32
  | CompleteVS (LV.Versionstamp 'LV.Complete)
  | IncompleteVS (LV.Versionstamp 'LV.Incomplete)
  deriving (Eq, Generic)

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
  deriving (Generic)
  deriving newtype (Eq, Hashable, Random)

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

maxTupleSize :: Seq (ByteString, Maybe [e]) -> Maybe Integer
maxTupleSize rows =
  case S.viewr $ S.sort $ fmap length . snd <$> rows of
    EmptyR -> Nothing
    _ :> a -> fromIntegral <$> a

operationSuccess :: Operation a -> Maybe a
operationSuccess = \case
  OperationSuccess a -> Just a
  _ -> Nothing

toEditableElem:: Elem -> EditableElem
toEditableElem = \case
  LT.None ->
    None
  LT.Tuple elems ->
    Tuple (toEditableElem <$> elems)
  LT.Bytes bs ->
    Bytes bs
  LT.Text t | T.any (\c -> c == '\n' || c == '\r') t ->
    MultiLineText t
  LT.Text t ->
    SingleLineText t
  LT.Int i ->
    Int i
  LT.Float f ->
    Float f
  LT.Double d ->
    Double d
  LT.Bool b ->
    Bool b
  LT.UUID a b c d ->
    UUID a b c d
  LT.CompleteVS vs ->
    CompleteVS vs
  LT.IncompleteVS vs ->
    IncompleteVS vs

fromEditableElem :: EditableElem -> Elem
fromEditableElem = \case
  None ->
    LT.None
  Tuple elems ->
    LT.Tuple (fromEditableElem <$> elems)
  Bytes bs ->
    LT.Bytes bs
  MultiLineText t ->
    LT.Text t
  SingleLineText t ->
    LT.Text t
  Int i ->
    LT.Int i
  Float f ->
    LT.Float f
  Double d ->
    LT.Double d
  Bool b ->
    LT.Bool b
  UUID a b c d ->
    LT.UUID a b c d
  CompleteVS vs ->
    LT.CompleteVS vs
  IncompleteVS vs ->
    LT.IncompleteVS vs

elemText :: EditableElem -> Maybe Text
elemText = \case
  SingleLineText t -> Just t
  MultiLineText t  -> Just t
  _                -> Nothing
