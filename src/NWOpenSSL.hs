{-# LANGUAGE OverloadedStrings #-}

module NWOpenSSL where

import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString      as B
import OpenSSL.EVP.Cipher

pkcs7pad :: Int -> BS.ByteString -> BS.ByteString
pkcs7pad len bytes = bytes `BS.append` paddingString
  where
    paddingString = BS.replicate (fromIntegral paddingByte) (fromIntegral paddingByte)
    paddingByte   = fromIntegral len - (BS.length bytes `mod` fromIntegral len)

aes128ecbDecrypt :: BS.ByteString -> BS.ByteString -> IO BS.ByteString
aes128ecbDecrypt key bytes = do cip <- getCipherByName "aes-128-ecb"
                                let strictKey = BS.toStrict key
                                case cip of
                                  Just c -> cipherLBS c strictKey B.empty Decrypt bytes
                                  Nothing -> return BS.empty

aes128ecbEncrypt :: BS.ByteString -> BS.ByteString -> IO BS.ByteString
aes128ecbEncrypt key bytes = do cip <- getCipherByName "aes-128-ecb"
                                let strictKey = BS.toStrict key
                                case cip of
                                  Just c -> cipherLBS c strictKey B.empty Encrypt bytes
                                  Nothing ->  return BS.empty

aes128cbcDecrypt :: BS.ByteString -> BS.ByteString -> BS.ByteString -> IO BS.ByteString
aes128cbcDecrypt _ = aes128ecbDecrypt

