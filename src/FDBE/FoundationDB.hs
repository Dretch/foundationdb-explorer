{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module FDBE.FoundationDB
  ( getClusterFilePath
  , getStatus
  , getSearchResult
  ) where

import           Control.Error.Util       (hush)
import           Control.Exception        (try)
import           Data.ByteString          (ByteString)
import           Data.Maybe               (fromMaybe)
import           Data.Sequence            (Seq)
import           Data.Text                (Text)
import qualified Data.Text                as T
import           Data.Text.Encoding       (decodeUtf8)
import           Data.Time.Clock          (NominalDiffTime)
import qualified Data.Time.Clock          as Clock
import           Data.Tuple.Extra         (both)
import           FoundationDB             (Database, Error, Range (..))
import qualified FoundationDB             as FDB
import           FoundationDB.Layer.Tuple (Elem (..), decodeTupleElems)
import qualified System.Process           as P

import           FDBE.Bytes               (textToBytes)
import           FDBE.State               (SearchRange (..), SearchResult (..))

getClusterFilePath :: Database -> IO (Maybe Text)
getClusterFilePath db = do
  FDB.runTransaction db $ do
    maybePath <- FDB.get "\xFF\xFF/cluster_file_path" >>= FDB.await
    pure $ fmap decodeUtf8 maybePath

getStatus :: Database -> IO Text
getStatus db = do
  clusterFilePath <- getClusterFilePath db
  let cArgs = fromMaybe [] ((\p -> ["-C", T.unpack p]) <$> clusterFilePath)
  T.pack <$> P.readProcess "fdbcli" (cArgs <> ["--exec", "status"]) ""

getSearchResult ::
     Database
  -> SearchRange
  -> IO (Either Error (NominalDiffTime, Seq SearchResult))
getSearchResult db SearchRange {..} = do
  try $ do
    startTime <- Clock.getCurrentTime
    rows <-
      FDB.runTransaction db $ do
        let range = FDB.keyRange (textToBytes searchFrom) (textToBytes searchTo)
            rangeLimit = Just $ fromIntegral searchLimit
        pairs <- FDB.getEntireRange range {rangeLimit}
        pure $ uncurry SearchResult . both decode <$> pairs
    endTime <- Clock.getCurrentTime
    pure (Clock.diffUTCTime endTime startTime, rows)

decode :: ByteString -> (ByteString, Maybe [Elem])
decode b = (b, hush $ decodeTupleElems b)
