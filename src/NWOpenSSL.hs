{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}

module NWOpenSSL where

import           Helpers
import           Codec.Crypto.SimpleAES (Key, Direction(..), Mode(..), crypt, randomKey)
import qualified Data.ByteString.Lazy as Lazy (fromStrict, toStrict)
import qualified Data.ByteString as ByteString (append, length, concat, pack, take)
import           Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as Char8 (replicate, pack, snoc, last, length, drop, take, unpack)
import           Data.Char (chr, ord)
import           Data.List (foldl', findIndex)
import           Data.Vector (toList)
import qualified Data.HashMap.Strict as HashMap (empty, insert, lookup)
import           Data.Maybe (fromJust)


import           System.Random.MWC
import           Debug.Trace

type IV = ByteString
type Encryptor = ByteString -> ByteString

pkcs7pad :: Int -> ByteString -> ByteString
pkcs7pad size bytes | byte > 0 = ByteString.append bytes paddingString
                    | otherwise = bytes
  where
    byte = size - ByteString.length bytes
    paddingString = Char8.pack . replicate byte $ chr byte

pkcs7unpad :: Int -> ByteString -> (Bool, ByteString)
pkcs7unpad size bytes = let end = ord . Char8.last $ bytes
                         in if end < size then unpad end bytes 
                                          else (False, "")
  where 
    unpad n bs = let contentLength = Char8.length bs - n
                     pad = Char8.drop contentLength bs
                     valid = all (== chr n) . Char8.unpack $ pad
                  in if valid then (True, Char8.take contentLength bs) else (False, "")

zeroIV :: ByteString
zeroIV = Char8.replicate 16 '\0'

randomAESKey :: IO ByteString
randomAESKey = randomBytes 16

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

randomPre :: IO ByteString
randomPre = do
  num <- uniformR (5, 10) =<< createSystemRandom
  randomBytes num

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

ecbKeySize :: Encryptor -> Int
ecbKeySize enc = head . filter (isECB enc) $ [1..64]

isECB :: Encryptor -> Int -> Bool
isECB enc size = let (a:b:_) = takeChunks size . enc . Char8.replicate (size * 2) $ '\0'
                  in a == b

ecbPrefixLength :: Encryptor -> Int -> Int
ecbPrefixLength enc blockSize = 
  let Just zeroBuffer = findIndex (uncurry (==)) $ zip spacers (tail spacers)
   in blockSize - zeroBuffer - 1
  where
    spacers = map (ByteString.take blockSize . enc . flip Char8.replicate '\0') [1..]

ecbPlaintextLength :: Encryptor -> Int
ecbPlaintextLength enc = let steps = map (ByteString.length . enc . flip Char8.replicate '\0') [0..]
                             Just paddingSize = findIndex (/= 0) . zipWith (-) (tail steps) $ steps
                          in head steps - paddingSize

breakECB :: Encryptor -> Int -> ByteString
breakECB enc blockSize = let padLength = blockSize - ecbPrefixLength enc blockSize
                          in foldl' (\bs i -> 
                              Char8.snoc bs (breakECBByte blockSize padLength enc i bs)) 
                              "" 
                              [1..ecbPlaintextLength enc]

breakECBByte :: Int -> Int -> Encryptor -> Int -> ByteString -> Char
breakECBByte blockSize padLength encryptor index bs = 
    let prefixLength = padLength + blockSize - (index - 1) `mod` blockSize - 1
        zeros = Char8.replicate prefixLength '\0'
        targetSize = padLength + ((1 + (index - 1) `quot` blockSize) * blockSize)
        target = ByteString.take targetSize $ encryptor zeros
        prefix = ByteString.append zeros bs
        dict = foldr
                  (\x m ->
                      let plaintext = Char8.snoc prefix (chr x)
                          ciphertext = encryptor plaintext
                       in HashMap.insert (ByteString.take targetSize ciphertext) x m)
                  HashMap.empty
                  [0..255]
    in chr . fromJust $ HashMap.lookup target dict

