{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module FDBE.FoundationDB
  ( getClusterFilePath
  , getStatus
  , EditableBytes
  , EditableElem(..)
  , toEditableElem
  , fromEditableElem
  , elemText
  , SearchRange(..)
  , SearchResult(..)
  , getSearchResult
  , getKeyValue
  , setKeyValue
  ) where

import           FDBE.Prelude

import qualified Data.Text                as T
import           Data.Text.Encoding       (decodeUtf8)
import           Data.Time.Clock          (NominalDiffTime)
import qualified Data.Time.Clock          as Clock
import           FoundationDB             (Database, Error, Range (..))
import qualified FoundationDB             as FDB
import           FoundationDB.Layer.Tuple (Elem, decodeTupleElems,
                                           encodeTupleElems)
import qualified FoundationDB.Layer.Tuple as LT
import qualified FoundationDB.Versionstamp as LV
import qualified System.Process           as P

import           FDBE.Bytes               (bytesToText, textToBytes)

getClusterFilePath :: Database -> IO (Maybe Text)
getClusterFilePath db =
  FDB.runTransaction db $ do
    maybePath <- FDB.get "\xFF\xFF/cluster_file_path" >>= FDB.await
    pure $ fmap decodeUtf8 maybePath

getStatus :: Database -> IO Text
getStatus db = do
  clusterFilePath <- getClusterFilePath db
  let cArgs = maybe [] (\p -> ["-C", T.unpack p]) clusterFilePath
  T.pack <$> P.readProcess "fdbcli" (cArgs <> ["--exec", "status"]) ""

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

data SearchRange =
  SearchRange
    { searchFrom    :: EditableBytes
    , searchTo      :: EditableBytes
    , searchLimit   :: Word
    , searchReverse :: Bool
    }

data SearchResult =
  SearchResult
    { resultKey   :: (ByteString, Maybe [Elem])
    , resultValue :: (ByteString, Maybe [Elem])
    }
  deriving (Eq)

getSearchResult
  :: Database
  -> SearchRange
  -> IO (Either Error (NominalDiffTime, Seq SearchResult))
getSearchResult db SearchRange {..} =
  try $ do
    startTime <- Clock.getCurrentTime
    pairs <- FDB.runTransaction db $ FDB.getEntireRange range
    -- force decoding here, so it gets included in the timed duration
    decodedPairs <- liftIO . evaluate . force $ both decode <$> pairs
    endTime <- Clock.getCurrentTime
    pure (Clock.diffUTCTime endTime startTime, uncurry SearchResult <$> decodedPairs)
  where
    range = Range
      { rangeBegin   = FDB.FirstGreaterOrEq $ encodeEditableBytes searchFrom
      , rangeEnd     = FDB.FirstGreaterOrEq $ encodeEditableBytes searchTo
      , rangeReverse = searchReverse
      , rangeLimit   = Just $ fromIntegral searchLimit
      }

encodeEditableBytes :: EditableBytes -> ByteString
encodeEditableBytes = either textToBytes (encodeTupleElems . fmap fromEditableElem)

decode :: ByteString -> (ByteString, Maybe [Elem])
decode b = (b, hush (decodeTupleElems b))

getKeyValue :: Database -> EditableBytes -> IO (Either Error (Maybe EditableBytes))
getKeyValue db key =
  try $ do
    val <- FDB.runTransaction db $ FDB.get (encodeEditableBytes key) >>= FDB.await
    case val of
      Nothing ->
        pure Nothing
      Just bs | Right tup <- decodeTupleElems bs ->
        pure . Just . Right $ toEditableElem <$> tup
      Just bs ->
        pure . Just . Left $ bytesToText bs

setKeyValue :: Database -> EditableBytes -> Maybe EditableBytes -> IO (Maybe Error)
setKeyValue db key value =
  fmap leftToMaybe <$> try $
    FDB.runTransaction db $
      case value of
        Nothing ->
          FDB.clear (encodeEditableBytes key)
        Just v ->
          FDB.set (encodeEditableBytes key) (encodeEditableBytes v)
