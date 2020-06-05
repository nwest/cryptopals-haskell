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
import Debug.Trace

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
aes128cbcEncrypt iv key bytes = BS.concat <$> (sequence . tail . foldl' f [return iv] $ takeChunks 16 bytes)
  where
    f bs a = bs ++ [g a =<< last bs]
    g xs ys = BS.take 16 <$> (aes128ecbEncrypt key . fromRight BS.empty . xorBytes xs $ ys)

aes128cbcDecrypt :: BS.ByteString -> BS.ByteString -> BS.ByteString -> IO BS.ByteString
aes128cbcDecrypt iv key bytes = BS.concat <$> (sequence . fst . foldl' f ([], [iv]) $ takeChunks 16 bytes)
  where
    f (bs, as) a = (bs ++ [g (last as) a], as ++ [a])
    g xs ys = do
      let decrypted = clean . BS.take 16 <$> aes128ecbDecrypt key (BS.append ys (BS.singleton 0))
      fromRight BS.empty . xorBytes xs <$> decrypted

randomAESKey :: IO BS.ByteString
randomAESKey = randomBytes 16

randomBytes :: Int -> IO BS.ByteString
randomBytes n = BS.pack . toList <$> (flip uniformVector n =<< createSystemRandom)

randomPad :: BS.ByteString -> IO BS.ByteString
randomPad bs = do
  gen <- createSystemRandom
  num <- randomBytes <$> uniformR (5, 10) gen
  before <- num
  after <- num
  let bs' = BS.append (BS.append before bs) after
  let chunks = takeChunks 16 bs'
      pChunks = init chunks ++ [pkcs7pad 16 (last chunks)]
  return . BS.concat $ pChunks

encryptionOracle :: BS.ByteString -> IO BS.ByteString
encryptionOracle bs = do
  input <- randomPad bs
  num <- uniformR (0 :: Int, 1) =<< createSystemRandom
  case num of
    0 -> trace ("ecb") ecb input
    1 -> trace ("cbc") cbc input
    _ -> return "derp"
  where
    ecb xs = flip aes128ecbEncrypt xs =<< randomAESKey
    cbc xs = do
      iv <- randomAESKey
      key <- randomAESKey
      aes128cbcEncrypt iv key xs
  
