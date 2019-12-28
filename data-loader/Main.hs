{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Main where

import           Control.Monad.IO.Class   (liftIO)
import           Data.Foldable            (forM_)
import           Data.Text                (Text)
import qualified Data.Text                as Text
import           Database.SQLite.Simple
import qualified FoundationDB             as FDB
import qualified FoundationDB.Layer.Tuple as T

data Video =
  Video
    { videoId        :: Text
    , videoTitle     :: Text
    , videoSubtitles :: Text
    , videoFetchedAt :: Text
    }
  deriving (Show)

instance FromRow Video where
  fromRow = Video <$> field <*> field <*> field <*> field

-- todo: make ValueTooLarge error say how big the value was and what the limit was
main :: IO ()
main = do
  conn <-
    open
      "/home/gareth/projects/youduo/youduo-server/server-backup/database.sqlite3"
  videos <- query_ conn "select * from videos" :: IO [Video]
  FDB.withFoundationDB FDB.defaultOptions $ \db -> do
    forM_ (zip [0 :: Integer ..] videos) $ \(i, Video {..}) -> do
      FDB.runTransaction db $ do
        if Text.length videoSubtitles > 1024 * 10
          then do
            liftIO $ putStrLn "Subtitles too big!"
          else do
            liftIO $ putStrLn $ "Inserting video " <> show i
            let key = T.encodeTupleElems ([T.Text videoId] :: [T.Elem])
            let value =
                  T.encodeTupleElems
                    [ T.Text videoTitle
                    , T.Text videoSubtitles
                    , T.Text videoFetchedAt
                    ]
            FDB.set key value
  close conn
