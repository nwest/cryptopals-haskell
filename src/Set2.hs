{-# LANGUAGE OverloadedStrings #-}

module Set2 where

import qualified Data.ByteString.Lazy as BS
import Data.ByteString.Internal (c2w)
import qualified Data.Set as S
import OpenSSL.EVP.Base64

import Helpers
import NWOpenSSL

num9 :: BS.ByteString
num9 = pkcs7pad 20 "YELLOW SUBMARINE"

num10 :: IO BS.ByteString
num10 = do
  bytes <- decodeBase64LBS . BS.filter (/= c2w '\n') <$> BS.readFile "./10.txt"
  let key = BS.pack . map c2w $ "YELLOW SUBMARINE"
      iv  = BS.replicate 16 0
  aes128cbcDecrypt iv key bytes

num11 :: IO String
num11 = do
  boxBytes <- encryptionOracle . BS.take (10 * 16) . BS.cycle $ "YELLOW SUBMARINE"
  let variations = map (\i -> takeChunks 16 . BS.drop i $ boxBytes) [3..12]
      sizes = map (\bs -> (length bs, S.size . S.fromList $ bs)) variations
      smaller = filter (\(a, b) -> b < a) sizes
  if null smaller then return "It's probably CBC"
                  else return "It's probably ECB"

