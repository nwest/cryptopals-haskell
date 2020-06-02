{-# LANGUAGE OverloadedStrings #-}

module Set2 where

import qualified Data.ByteString.Lazy as BS
import Data.ByteString.Internal (c2w)

import NWOpenSSL

num9 :: BS.ByteString
num9 = pkcs7pad 20 "YELLOW SUBMARINE"

num10 :: IO BS.ByteString
num10 = do
  bytes <- BS.filter (/= c2w '\n') <$> BS.readFile "./10.txt"
  let iv = BS.replicate 16 0
  aes128cbcDecrypt iv "YELLOW SUBMARINE" bytes
