module FoundationDBUtil
    ( getClusterFilePath
    , getStatus
    , getSearchResult
    ) where

import Control.Exception (try)
import Control.Monad (void)
import Data.Bits (shiftL, (.|.))
import Data.ByteString (ByteString)
import qualified Data.ByteString.Internal as B
import qualified Data.ByteString as B
import qualified Data.Char as Char
import qualified Data.List as List
import Data.Maybe (fromMaybe)
import Data.Sequence (Seq)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Data.Void (Void)
import Data.Word (Word8)
import qualified Text.Megaparsec as M
import Text.Megaparsec ((<|>))
import qualified Text.Megaparsec.Char as MC
import qualified Text.Printf as Printf
import qualified System.Process as P
import FoundationDB (Database, Error, Range (..))
import qualified FoundationDB as FDB

import State (SearchRange (..), SearchResult (..))

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

getSearchResult :: Database -> SearchRange -> IO (Either Error (Seq SearchResult))
getSearchResult db SearchRange{..} = do
    try $ FDB.runTransaction db $ do
        let range = FDB.keyRange (keyFromText searchFrom) (keyFromText searchTo)
        pairs <- FDB.getEntireRange range{rangeLimit = Just 100} -- todo: make configurable
        pure $ (\(k, v) -> SearchResult{resultKey = keyToText k, resultValue = keyToText v}) <$> pairs

type Parser = M.Parsec Void Text

keyFromText :: Text -> ByteString
keyFromText key =
    case M.parse parser "" key of
        Left _   -> error "Key parser failed - this should not be possible!"
        Right bs -> bs
    where
        parser :: Parser ByteString
        parser = B.concat <$> M.many (escapedByte <|> escapedSlash <|> regularChar)

        escapedByte :: Parser ByteString
        escapedByte = do
            void $ MC.string "\\x"
            a <- MC.hexDigitChar
            b <- MC.hexDigitChar
            pure $ B.singleton $ (charToBase16 a `shiftL` 4) .|. charToBase16 b
        
        charToBase16 :: Char -> Word8
        charToBase16 = fromIntegral . fromMaybe 0 . flip List.elemIndex "0123456789abcdef" . Char.toLower

        escapedSlash :: Parser ByteString
        escapedSlash = encodeUtf8 "\\" <$ MC.string "\\\\"
        
        regularChar :: Parser ByteString
        regularChar = encodeUtf8 . T.singleton <$> M.anySingle

keyToText :: ByteString -> Text
keyToText = T.concat . fmap mapChar . B.unpack
  where
    mapChar w
     | Char.isPrint (B.w2c w) = T.singleton $ B.w2c w
     | otherwise              = T.pack $ Printf.printf "\\x%02x" w

