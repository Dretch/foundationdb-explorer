{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module FDBE.FoundationDB
  ( getClusterFilePath
  , getStatus
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
import           FoundationDB.Layer.Tuple (Elem (..), decodeTupleElems,
                                           encodeTupleElems)
import qualified System.Process           as P

import           FDBE.Bytes               (bytesToText, textToBytes)
import           FDBE.State               (EditableBytes, SearchRange (..),
                                           SearchResult (..))

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
      { rangeBegin   = FDB.FirstGreaterOrEq $ selectorToBytes searchFrom
      , rangeEnd     = FDB.FirstGreaterOrEq $ selectorToBytes searchTo
      , rangeReverse = searchReverse
      , rangeLimit   = Just $ fromIntegral searchLimit
      }

selectorToBytes :: EditableBytes -> ByteString
selectorToBytes = either textToBytes encodeTupleElems

decode :: ByteString -> (ByteString, Maybe [Elem])
decode b = (b, hush $ decodeTupleElems b)

getKeyValue :: Database -> EditableBytes -> IO (Either Error (Maybe EditableBytes))
getKeyValue db key =
  try $ do
    val <- FDB.runTransaction db $ FDB.get (selectorToBytes key) >>= FDB.await
    case val of
      Nothing ->
        pure Nothing
      Just bs | Right tup <- decodeTupleElems bs ->
        pure . Just $ Right tup
      Just bs ->
        pure . Just . Left $ bytesToText bs

setKeyValue :: Database -> EditableBytes -> Maybe EditableBytes -> IO (Maybe Error)
setKeyValue db key value =
  fmap leftToMaybe <$> try $
    FDB.runTransaction db $
      case value of
        Nothing ->
          FDB.clear (selectorToBytes key)
        Just v ->
          FDB.set (selectorToBytes key) (selectorToBytes v)
