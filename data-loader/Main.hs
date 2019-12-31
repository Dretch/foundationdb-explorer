{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Main where

import           Control.Monad.IO.Class    (liftIO)
import           Data.Foldable             (forM_)
import           Data.Text                 (Text)
import qualified Data.Text                 as Text
import qualified Data.UUID                 as UUID
import qualified Data.UUID.V4              as UUID
import           Database.SQLite.Simple
import qualified FoundationDB              as FDB
import qualified FoundationDB.Layer.Tuple  as T
import qualified FoundationDB.Versionstamp as V

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
  FDB.withFoundationDB FDB.defaultOptions $ \db -> do
    loadDatatypes db
    loadSubtitles db

loadDatatypes :: FDB.Database -> IO ()
loadDatatypes db = do
  uuid <- (\(a, b, c, d) -> T.UUID a b c d) . UUID.toWords <$> UUID.nextRandom
  FDB.runTransaction db $ do
    set "\x01.none" T.None
    set "\x01.text" $ T.Text "abc"
    set "\x01.int" $ T.Int 123
    set "\x01.float" $ T.Float 123.456
    set "\x01.double" $ T.Double 123.456
    set "\x01.bool" $ T.Bool True
    set "\x01.bytes" $ T.Bytes "\\xF1\\xF2\\xF3"
    set "\x01.nested" $ T.Tuple [T.Text "a", T.Tuple [T.Text "b", T.Text "c"]]
    set "\x01.uuid" uuid
    set "\x01.complete-versionstamp" $
      T.CompleteVS $
      V.CompleteVersionstamp (V.TransactionVersionstamp 123456789 123) 456
    set "\x01.incomplete-versionstamp" $
      T.IncompleteVS $ V.IncompleteVersionstamp 123
  where
    set key value = do
      FDB.set (T.encodeTupleElems [T.Text key]) (T.encodeTupleElems [value])

loadSubtitles :: FDB.Database -> IO ()
loadSubtitles db = do
  conn <-
    open
      "/home/gareth/projects/youduo/youduo-server/server-backup/database.sqlite3"
  videos <- query_ conn "select * from videos" :: IO [Video]
  close conn
  forM_ (zip [0 :: Integer ..] videos) $ \(i, Video {..}) -> do
    FDB.runTransaction db $ do
      if Text.length videoSubtitles > 1024 * 10
        then do
          liftIO $ putStrLn "Subtitles too big!"
        else do
          liftIO $ putStrLn $ "Inserting video " <> show i
          let key = T.encodeTupleElems [T.Text videoId]
          let value =
                T.encodeTupleElems
                  [ T.Text videoTitle
                  , T.Text videoSubtitles
                  , T.Text videoFetchedAt
                  ]
          FDB.set key value
