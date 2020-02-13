module BytesSpec (spec) where

import           FDBE.Bytes      (bytesToText, textToBytes)
import           Test.Hspec      (Spec, it)
import           Test.QuickCheck (property)
import Test.QuickCheck.Instances.ByteString ()

spec :: Spec
spec = do
  it "textToBytes . bytesToText = id" $ do
    property (\bs -> (textToBytes . bytesToText) bs == bs)
