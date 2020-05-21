{-# LANGUAGE OverloadedStrings #-}

import qualified Test.Tasty
import Test.Tasty.Hspec
import Set1
import Control.Monad (join)
import Data.ByteString.Internal (c2w)
import qualified Data.ByteString.Lazy as BS

main :: IO ()
main = do
    test <- testSpec "cryptopals" spec
    Test.Tasty.defaultMain test

spec :: Spec
spec = parallel $ do
  describe "Set1" $ do
    describe "Challenge 1" $ do
      it "translates hex string to correct base64 string" $ do
        let expected = Right . Base64String $ "SSdtIGtpbGxpbmcgeW91ciBicmFpbiBsaWtlIGEgcG9pc29ub3VzIG11c2hyb29t"
            result = hexToBase64 $ HexString "49276d206b696c6c696e6720796f757220627261696e206c696b65206120706f69736f6e6f7573206d757368726f6f6d"
        result `shouldBe` expected

    describe "Challenge 2" $ do
      it "XORs two strings of data" $ do
        let expected  = Right . HexString $ "746865206b696420646f6e277420706c6179"
            a = decodeHex . HexString $ "1c0111001f010100061a024b53535009181c"
            b = decodeHex . HexString $ "686974207468652062756c6c277320657965"
            result = bytesToHex <$> join (xorBytes <$> a <*> b)
        result `shouldBe` expected

    describe "Challenge 3" $ do

      it "converts a byte string back to a raw string" $ do
        let hexString = HexString $ "6669747465722c206861707065722c206d6f72652070726f64756374697665"
            result = hexToRaw hexString
            expected = "fitter, happer, more productive"
        result `shouldBe` expected

      it "computes a single char based XOR" $ do
        let input = hexToBytes . HexString $ "1b37373331363f78151b7f2b783431333d78397828372d363c78373e783a393b3736"
            (_, _, result) = bestSingleCharXOR input
            expected = "Cooking MC's like a pound of bacon"
        result `shouldBe` expected

