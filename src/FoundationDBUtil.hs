{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module FoundationDBUtil
  ( getClusterFilePath
  , getStatus
  , getSearchResult
  ) where

import           Bytes                    (textToBytes)
import           Control.Error.Util       (hush)
import           Control.Exception        (try)
import           Data.ByteString          (ByteString)
import           Data.Maybe               (fromMaybe)
import           Data.Sequence            (Seq)
import           Data.Text                (Text)
import qualified Data.Text                as T
import           Data.Text.Encoding       (decodeUtf8)
import           FoundationDB             (Database, Error, Range (..))
import qualified FoundationDB             as FDB
import           FoundationDB.Layer.Tuple (Elem (..), decodeTupleElems)
import           State                    (SearchRange (..), SearchResult (..))
import qualified System.Process           as P

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
     Database -> SearchRange -> IO (Either Error (Seq SearchResult))
getSearchResult db SearchRange {..} = do
  try $
    FDB.runTransaction db $ do
      let range = FDB.keyRange (textToBytes searchFrom) (textToBytes searchTo)
      pairs <- FDB.getEntireRange range {rangeLimit = Just 100} -- todo: make configurable
      pure $
        (\(k, v) -> SearchResult {resultKey = decode k, resultValue = decode v}) <$>
        pairs

decode :: ByteString -> (ByteString, Maybe [Elem])
decode b = (b, hush $ decodeTupleElems b)
