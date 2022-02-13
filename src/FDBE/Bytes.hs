{-# LANGUAGE LambdaCase #-}

module FDBE.Bytes
  ( textToBytes,
    bytesToText,
  )
where

import Data.Bits (shiftL, (.|.))
import qualified Data.ByteString as B
import qualified Data.ByteString.Internal as B
import qualified Data.Char as Char
import qualified Data.List as List
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import FDBE.Prelude
import Text.Megaparsec ((<|>))
import qualified Text.Megaparsec as M
import qualified Text.Megaparsec.Char as MC

type Parser = M.Parsec Void Text

textToBytes :: Text -> ByteString
textToBytes key =
  case M.parse parser "" key of
    Left _ -> error "Bytes.textToBytes failed - this should not be possible!"
    Right bs -> bs
  where
    parser :: Parser ByteString
    parser =
      B.concat
        <$> M.many (M.try escapedByte <|> M.try escapedSlash <|> regularChar)
    escapedByte :: Parser ByteString
    escapedByte = do
      void $ MC.string "\\x"
      a <- MC.hexDigitChar
      b <- MC.hexDigitChar
      pure $ B.singleton $ (fromBase16 a `shiftL` 4) .|. fromBase16 b
    fromBase16 :: Char -> Word8
    fromBase16 =
      fromIntegral
        . fromMaybe 0
        . flip List.elemIndex "0123456789abcdef"
        . Char.toLower
    escapedSlash :: Parser ByteString
    escapedSlash = encodeUtf8 "\\" <$ MC.string "\\\\"
    regularChar :: Parser ByteString
    regularChar = encodeUtf8 . T.singleton <$> M.anySingle

bytesToText :: ByteString -> Text
bytesToText = T.pack . encodeSpecials . B.unpack
  where
    encodeSpecials :: [Word8] -> [Char]
    encodeSpecials = \case
      [] -> []
      (w : w' : ws)
        | slash w && (slash w' || not (visible w')) ->
          '\\' : '\\' : encodeSpecials (w' : ws)
      (w : ws)
        | visible w ->
          B.w2c w : encodeSpecials ws
      (w : ws) ->
        printf "\\x%02x" w <> encodeSpecials ws
    slash w =
      B.w2c w == '\\'
    visible w =
      B.w2c w >= ' ' && B.w2c w <= '~'
