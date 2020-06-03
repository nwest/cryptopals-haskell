{-# LANGUAGE OverloadedStrings #-}

module NWOpenSSL where

import Helpers

import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString      as B
import Data.Either (fromRight)
import Data.List (foldl')
import Data.Vector (toList)
import OpenSSL.EVP.Cipher
import System.Random.MWC

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


aes128cbcEncrypt :: BS.ByteString -> BS.ByteString -> BS.ByteString -> IO BS.ByteString
aes128cbcEncrypt iv key bytes = BS.concat <$> (sequence . fst . foldl' f ([], [iv]) $ takeChunks 16 bytes)
  where
    f (bs, as) a = (bs ++ [g (last as) a], as ++ [a])
    g xs ys = do
      let encrypted = clean . BS.take 16 <$> aes128ecbEncrypt key (BS.append ys (BS.singleton 0))
      fromRight BS.empty . xorBytes xs <$> encrypted

aes128cbcDecrypt :: BS.ByteString -> BS.ByteString -> BS.ByteString -> IO BS.ByteString
aes128cbcDecrypt iv key bytes = BS.concat <$> (sequence . fst . foldl' f ([], [iv]) $ takeChunks 16 bytes)
  where
    f (bs, as) a = (bs ++ [g (last as) a], as ++ [a])
    g xs ys = do
      let decrypted = clean . BS.take 16 <$> aes128ecbDecrypt key (BS.append ys (BS.singleton 0))
      fromRight BS.empty . xorBytes xs <$> decrypted

randomAESKey :: IO BS.ByteString
randomAESKey = BS.pack . toList <$> (flip uniformVector 16 =<< createSystemRandom)

