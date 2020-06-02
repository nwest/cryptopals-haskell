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
        let expected = Right $ "SSdtIGtpbGxpbmcgeW91ciBicmFpbiBsaWtlIGEgcG9pc29ub3VzIG11c2hyb29t"
            result = hexToBase64 $ "49276d206b696c6c696e6720796f757220627261696e206c696b65206120706f69736f6e6f7573206d757368726f6f6d"
        result `shouldBe` expected

    describe "Challenge 2" $ do
      it "XORs two strings of data" $ do
        let expected  = Right $ "746865206b696420646f6e277420706c6179"
            a = decodeHex "1c0111001f010100061a024b53535009181c"
            b = decodeHex "686974207468652062756c6c277320657965"
            result = bytesToHex <$> join (xorBytes <$> a <*> b)
        result `shouldBe` expected

    describe "Challenge 3" $ do

      it "converts a byte string back to a raw string" $ do
        let hexString = "6669747465722c206861707065722c206d6f72652070726f64756374697665"
            result = hexToRaw hexString
            expected = "fitter, happer, more productive"
        result `shouldBe` expected

      it "computes a single char based XOR" $ do
        let input = hexToBytes "1b37373331363f78151b7f2b783431333d78397828372d363c78373e783a393b3736"
            (_, _, result) = bestSingleCharXOR input
            expected = "Cooking MC's like a pound of bacon"
        result `shouldBe` expected

      it "can do repeating key xor" $ do
        let expected = "0b3637272a2b2e63622c2e69692a23693a2a3c6324202d623d63343c2a26226324272765272a282b2f20430a652e2c652a3124333a653e2b2027630c692b20283165286326302e27282f"
            result = bytesToHex . repeatKeyXOR "ICE" $ "Burning 'em, if you ain't quick and nimble\nI go crazy when I hear a cymbal"
        result `shouldBe` expected


