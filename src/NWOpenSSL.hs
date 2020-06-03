{-# LANGUAGE OverloadedStrings #-}

module NWOpenSSL where

import Helpers

import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString      as B
import Data.Either (fromRight)
import Data.List (foldl')
import OpenSSL.EVP.Cipher

pkcs7pad :: Int -> BS.ByteString -> BS.ByteString
pkcs7pad len bytes = bytes `BS.append` paddingString
  where
    paddingString = BS.replicate (fromIntegral paddingByte) (fromIntegral paddingByte)
    paddingByte   = fromIntegral len - (BS.length bytes `mod` fromIntegral len)

aes128ecbEncrypt :: BS.ByteString -> BS.ByteString -> IO BS.ByteString
aes128ecbEncrypt key bytes = do cip <- getCipherByName "aes-128-ecb"
                                let strictKey = BS.toStrict key
                                case cip of
                                  Just c -> cipherLBS c strictKey B.empty Encrypt bytes
                                  Nothing ->  return BS.empty

aes128ecbDecrypt :: BS.ByteString -> BS.ByteString -> IO BS.ByteString
aes128ecbDecrypt key bytes = do cip <- getCipherByName "aes-128-ecb"
                                let strictKey = BS.toStrict key
                                case cip of
                                  Just c -> cipherLBS c strictKey B.empty Decrypt bytes
                                  Nothing -> return BS.empty


aes128cbcDecrypt :: BS.ByteString -- IV
                 -> BS.ByteString -- Key
                 -> BS.ByteString -- Bytes
                 -> IO BS.ByteString
aes128cbcDecrypt iv key bytes = do let chunked = takeChunks 16 bytes
                                   foldl' i (return BS.empty) . fst . foldl' f ([], [iv]) $ chunked
  where
    f (bs, as) a = let bs' = bs ++ [g (last as) a]
                       as' = as ++ [a]
                    in (bs', as')
    g xs ys = do
      let decrypted = clean . BS.take 16 <$> aes128ecbDecrypt key (BS.append ys (BS.singleton 0))
      fromRight BS.empty . xorBytes xs <$> decrypted
    clean = BS.pack . BS.unpack
    i d c = do
      current <- d
      next <- c
      if BS.null current then c
                         else return (BS.append current next)
