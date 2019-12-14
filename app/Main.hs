module Main where

import Control.Monad (void)
import Data.Text (Text)
import qualified Data.Text.IO as T
import Data.Text.Encoding (decodeUtf8)
import qualified Data.ByteString as B
import System.Exit (die)

import GI.Gtk.Declarative.App.Simple
import qualified FoundationDB as FDB

import Splash (State (..), app)

main :: IO ()
main = do
    path <- getClusterFilePath
    State s <- run $ app path
    putStrLn $ "Finished: " <> show s

getClusterFilePath :: IO (Maybe Text)
getClusterFilePath = do
    FDB.withFoundationDB FDB.defaultOptions $ \db -> do
        FDB.runTransaction db $ do
            maybePath <- FDB.get "\xFF\xFF/cluster_file_path" >>= FDB.await
            pure $ fmap decodeUtf8 maybePath
