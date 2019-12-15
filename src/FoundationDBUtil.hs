module FoundationDBUtil
    ( getClusterFilePath
    , getStatus
    ) where

import Data.Text (Text, unpack, pack)
import Data.Text.Encoding (decodeUtf8)
import qualified System.Process as P
import qualified FoundationDB as FDB

getClusterFilePath :: IO (Maybe Text)
getClusterFilePath = do
    FDB.withFoundationDB FDB.defaultOptions $ \db -> do
        FDB.runTransaction db $ do
            maybePath <- FDB.get "\xFF\xFF/cluster_file_path" >>= FDB.await
            pure $ fmap decodeUtf8 maybePath

getStatus :: Text -> IO Text
getStatus clusterFilePath = do
    pack <$> P.readProcess "fdbcli" ["-C", unpack clusterFilePath, "--exec", "status"] ""