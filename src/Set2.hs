{-# LANGUAGE OverloadedStrings #-}

module Set2 where

import qualified Data.ByteString.Lazy as BS
import Data.ByteString.Internal (c2w)
import OpenSSL.EVP.Base64

import NWOpenSSL

num9 :: BS.ByteString
num9 = pkcs7pad 20 "YELLOW SUBMARINE"

num10 :: IO BS.ByteString
num10 = do
  bytes <- decodeBase64LBS . BS.filter (/= c2w '\n') <$> BS.readFile "./10.txt"
  let key = BS.pack . map c2w $ "YELLOW SUBMARINE"
      iv  = BS.replicate 16 0
  aes128cbcDecrypt iv key bytes

