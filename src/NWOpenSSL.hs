{-# LANGUAGE OverloadedStrings #-}

module NWOpenSSL where

import           Helpers
import           Codec.Crypto.SimpleAES (Key, Direction(..), Mode(..), crypt, randomKey)
import qualified Data.ByteString.Lazy as Lazy (fromStrict, toStrict)
import qualified Data.ByteString as ByteString (append, length, concat, pack)
import           Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as Char8 (replicate, pack)
import           Data.Char (chr)
import           Data.List (foldl')
import           Data.Vector (toList)
import           System.Random.MWC
import           Debug.Trace

type IV = ByteString

pkcs7pad :: Int -> ByteString -> ByteString
pkcs7pad size bytes | byte > 0 = ByteString.append bytes paddingString
                    | otherwise = bytes
  where
    byte = size - ByteString.length bytes
    paddingString = Char8.pack . replicate byte $ chr byte

zeroIV :: ByteString
zeroIV = Char8.replicate 16 '\0'

encryptECB :: Key -> ByteString -> ByteString
encryptECB key = Lazy.toStrict . crypt ECB key zeroIV Encrypt . Lazy.fromStrict

decryptECB :: Key -> ByteString -> ByteString
decryptECB key = Lazy.toStrict . crypt ECB key zeroIV Decrypt . Lazy.fromStrict

encryptCBC :: IV -> Key -> ByteString -> ByteString
encryptCBC iv key = ByteString.concat . foldl' f [] . takeChunks 16
  where
    f bs b = bs ++ [encryptECB key . xorBytes b $ lastOr iv bs]

decryptCBC :: IV -> Key -> ByteString -> ByteString
decryptCBC iv key = ByteString.concat . fst . foldl' f ([], []) . takeChunks 16
   where
     f (bs, as) a = (bs ++ [xorBytes (lastOr iv as) . decryptECB key $ a], as ++ [a])

lastOr :: a -> [a] -> a
lastOr a [] = a
lastOr _ as = last as

randomBytes :: Int -> IO ByteString
randomBytes n = ByteString.pack . toList <$> (flip uniformVector n =<< createSystemRandom)
 
randomPad :: ByteString -> IO ByteString
randomPad bs = do
  gen <- createSystemRandom
  num <- randomBytes <$> uniformR (5, 10) gen
  before <- num
  after <- num
  let bs' = ByteString.append (ByteString.append before bs) after
  let chunks = takeChunks 16 bs'
      pChunks = init chunks ++ [pkcs7pad 16 (last chunks)]
  return . ByteString.concat $ pChunks

encryptionOracle :: ByteString -> IO ByteString
encryptionOracle bs = do 
  useECB <- uniformR (True, False) =<< createSystemRandom
  randomPad bs >>= if useECB then trace "ecb" ecb 
                             else trace "cbc" cbc
  where
    ecb xs = flip encryptECB xs <$> randomKey
    cbc xs = flip (uncurry encryptCBC) xs <$> ((,) <$> randomKey <*> randomKey)
   
ecbEncryptionOracle :: ByteString -> ByteString -> IO ByteString
ecbEncryptionOracle key bs = encryptECB key <$> randomPad bs

