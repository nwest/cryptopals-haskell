{-# LANGUAGE OverloadedStrings #-}

module Set2 where

import qualified Data.Set as S
import           Codec.Crypto.SimpleAES (Key)
import qualified Data.ByteString.Lazy as Lazy (cycle, take, toStrict)
import qualified Data.ByteString as ByteString (drop, readFile, append)
import           Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as Char8 (pack, filter)
import qualified Data.ByteString.Base64 as Base64 (decodeLenient)

import NWOpenSSL
import Helpers

num9 :: ByteString
num9 = pkcs7pad 20 "YELLOW SUBMARINE"

num10 :: IO ByteString
num10 = do
  bytes <- Base64.decodeLenient . Char8.filter (/= '\n') <$> ByteString.readFile "./10.txt"
  return . decryptCBC zeroIV (Char8.pack "YELLOW SUBMARINE") $ bytes

num11 :: IO String
num11 = do
  boxBytes <- encryptionOracle . Lazy.toStrict $ (Lazy.take (10 * 16) . Lazy.cycle $ "YELLOW SUBMARINE")
  let variations = map (\i -> takeChunks 16 . ByteString.drop i $ boxBytes) [3..12]
      sizes = map (\bs -> (length bs, S.size . S.fromList $ bs)) variations
      smaller = filter (\(a, b) -> b < a) sizes
  if null smaller then return "It's probably CBC"
                  else return "It's probably ECB"


num12Bytes :: ByteString
num12Bytes = Base64.decodeLenient "Um9sbGluJyBpbiBteSA1LjAKV2l0aCBteSByYWctdG9wIGRvd24gc28gbXkgaGFpciBjYW4gYmxvdwpUaGUgZ2lybGllcyBvbiBzdGFuZGJ5IHdhdmluZyBqdXN0IHRvIHNheSBoaQpEaWQgeW91IHN0b3A/IE5vLCBJIGp1c3QgZHJvdmUgYnkK"

num12Encryptor :: Key -> Encryptor
num12Encryptor key bs = encryptECB key $ ByteString.append bs num12Bytes

num12 :: IO ()
num12 = do
  key <- randomAESKey
  let encryptor = num12Encryptor key
      keySize = ecbKeySize encryptor
  print keySize
  print $ isECB encryptor keySize
  print $ breakECB encryptor keySize

