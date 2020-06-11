{-# LANGUAGE OverloadedStrings #-}

module Set2 where

import qualified Data.ByteString.Lazy as BS
import Data.ByteString.Internal (c2w)
import qualified Data.Set as S
import Data.List (elemIndex, findIndex)
import OpenSSL.EVP.Base64

import qualified Data.ByteString as ByteString (append, concat, length, take, findIndex)
import           Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as Char8 (pack, replicate, snoc, unpack)

import Helpers
import NWOpenSSL

type Encryptor = ByteString -> ByteString

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
  boxBytes <- encryptionOracle (BS.take (10 * 16) . BS.cycle $ "YELLOW SUBMARINE")
  let variations = map (\i -> takeChunks 16 . BS.drop i $ boxBytes) [3..12]
      sizes = map (\bs -> (length bs, S.size . S.fromList $ bs)) variations
      smaller = filter (\(a, b) -> b < a) sizes
  if null smaller then return "It's probably CBC"
                  else return "It's probably ECB"


num12Bytes :: BS.ByteString
num12Bytes = decodeBase64LBS "Um9sbGluJyBpbiBteSA1LjAKV2l0aCBteSByYWctdG9wIGRvd24gc28gbXkgaGFpciBjYW4gYmxvdwpUaGUgZ2lybGllcyBvbiBzdGFuZGJ5IHdhdmluZyBqdXN0IHRvIHNheSBoaQpEaWQgeW91IHN0b3A/IE5vLCBJIGp1c3QgZHJvdmUgYnkK"

num12 :: IO ()
num12 = do
  key <- randomAESKey
  let sizeTests = map (\b -> BS.append b b) . map (BS.pack . map c2w . flip replicate 'A') $ [1..64]
  testBytes <- map (\(i, bs)-> BS.take (i * 2) bs) . zip [1..] <$> (sequence . map (aes128ecbEncrypt key) $ sizeTests)
  let keySize = ecbKeySize testBytes
  print keySize
  let rawDictionaryValues = map (BS.snoc (BS.replicate (fromIntegral . pred $ keySize) (c2w 'A'))) $ [0..255]
  encryptedDictionaryValues <- sequence . map (aes128ecbEncrypt key) $ rawDictionaryValues

  let dictionary = map (BS.take (fromIntegral keySize)) $ encryptedDictionaryValues
      as = BS.pack . map c2w . replicate (pred keySize) $ 'A' 

  bss <- map (BS.take (fromIntegral keySize)) <$> (sequence . map (aes128ecbEncrypt key) . map (BS.snoc as) $ BS.unpack num12Bytes)

  let secret = BS.pack . map (fromIntegral . f . flip elemIndex dictionary) $ bss
  print secret
    where
      f Nothing = 0
      f (Just x) = x

ecbKeySize :: [BS.ByteString] -> Int
ecbKeySize bs = let sizes = map (\(i, xs) -> (i, setSize i xs)) . zip [1..] $ bs
                    smaller = filter (\(_, (b, c)) -> b > c) sizes
                 in if null smaller then 0
                                    else fst . head $ smaller
                  where
                    setSize i xs = let chunks = takeChunks i xs
                                   in (length chunks, S.size . S.fromList $ chunks)

inferEBCInfo :: Encryptor -> (Int, Int)
inferEBCInfo f =
    let lengths = map (\i -> ByteString.length $ f (Char8.replicate i '\0')) [0..]
        transitions = zipWith (-) (drop 1 lengths) lengths
        chunkSize = head $ dropWhile (== 0) transitions
        Just offset = findIndex (/= 0) transitions
        plaintextLength = head lengths - offset
    in (chunkSize, plaintextLength)

