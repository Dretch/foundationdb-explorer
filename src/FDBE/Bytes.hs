{-# LANGUAGE OverloadedStrings #-}

module FDBE.Bytes
  ( textToBytes
  , bytesToText
  ) where

import           Control.Monad            (void)
import           Data.Bits                (shiftL, (.|.))
import           Data.ByteString          (ByteString)
import qualified Data.ByteString          as B
import qualified Data.ByteString.Internal as B
import qualified Data.Char                as Char
import qualified Data.List                as List
import           Data.Maybe               (fromMaybe)
import           Data.Text                (Text)
import qualified Data.Text                as T
import           Data.Text.Encoding       (encodeUtf8)
import           Data.Void                (Void)
import           Data.Word                (Word8)
import           Text.Megaparsec          ((<|>))
import qualified Text.Megaparsec          as M
import qualified Text.Megaparsec.Char     as MC
import qualified Text.Printf              as Printf

type Parser = M.Parsec Void Text

textToBytes :: Text -> ByteString
textToBytes key =
  case M.parse parser "" key of
    Left _   -> error "Bytes.textToBytes failed - this should not be possible!"
    Right bs -> bs
  where
    parser :: Parser ByteString
    parser =
      B.concat <$>
      M.many (M.try escapedByte <|> M.try escapedSlash <|> regularChar)
    escapedByte :: Parser ByteString
    escapedByte = do
      void $ MC.string "\\x"
      a <- MC.hexDigitChar
      b <- MC.hexDigitChar
      pure $ B.singleton $ (fromBase16 a `shiftL` 4) .|. fromBase16 b
    fromBase16 :: Char -> Word8
    fromBase16 =
      fromIntegral .
      fromMaybe 0 . flip List.elemIndex "0123456789abcdef" . Char.toLower
    escapedSlash :: Parser ByteString
    escapedSlash = encodeUtf8 "\\" <$ MC.string "\\\\"
    regularChar :: Parser ByteString
    regularChar = encodeUtf8 . T.singleton <$> M.anySingle

bytesToText :: ByteString -> Text
bytesToText = T.concat . fmap mapChar . B.unpack
  where
    mapChar w
      | Char.isPrint (B.w2c w) = T.singleton $ B.w2c w
      | otherwise = T.pack $ Printf.printf "\\x%02x" w
