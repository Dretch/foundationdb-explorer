module FDBE.BytesSpec (spec) where

import qualified Data.ByteString as B
import Data.Text (Text)
import qualified Data.Text as T
import FDBE.Bytes (bytesToText, textToBytes)
import Test.Hspec (Spec, it, shouldBe)
import Test.QuickCheck (property)
import Test.QuickCheck.Instances.ByteString ()

spec :: Spec
spec = do
  it "regular ascii characters should not be encoded" $ do
    shouldNotBeEncoded regulars
  it "slash syntax should be encoded as byte values" $ do
    textToBytes "\\xf4" `shouldBe` B.pack [0xF4]
    textToBytes "\\xF4" `shouldBe` B.pack [0xF4]
  it "incomplete slash syntax should not be encoded (to allow entering escape sequences easily)" $ do
    shouldNotBeEncoded "\\xfH"
    shouldNotBeEncoded "\\xf"
    shouldNotBeEncoded "\\xF"
    shouldNotBeEncoded "\\x-"
    shouldNotBeEncoded "\\x"
    shouldNotBeEncoded "\\a"
    shouldNotBeEncoded "\\"
  it "double slash should be encoded as single slash" $ do
    textToBytes "\\\\" `shouldBe` B.pack [0x5C]
    textToBytes "\\\\a" `shouldBe` B.pack [0x5C, 0x61]
    textToBytes "\\\\x" `shouldBe` B.pack [0x5C, 0x78]
  it "slash byte should be doubled when followed by another slash (to avoid amibuity)" $ do
    bytesToText (B.pack [0x5C, 0x5C]) `shouldBe` "\\\\\\"
    bytesToText (B.pack [0x5C, 0x5C, 0x61]) `shouldBe` "\\\\\\a"
    bytesToText (B.pack [0x5C, 0x5C, 0x5C, 0x61]) `shouldBe` "\\\\\\\\\\a"
  it "property: textToBytes . bytesToText = id" $ do
    property (\bs -> (textToBytes . bytesToText) bs == bs)
  where
    regulars = T.pack (filter (/= '\\') [' ' .. '~'])

shouldNotBeEncoded :: Text -> IO ()
shouldNotBeEncoded t =
  bytesToText (textToBytes t) `shouldBe` t
