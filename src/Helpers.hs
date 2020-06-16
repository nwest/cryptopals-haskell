{-# LANGUAGE OverloadedStrings #-}

module Helpers where

import           Data.Word (Word8)

import qualified Data.ByteString as ByteString (foldl', length, singleton, append, zipWith, pack, concat)
import           Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as Char8 (unpack)

import qualified Data.ByteString.Base16 as BS16
import qualified Data.ByteString.Base64 as BS64
import           Data.Bits (xor)

type HexString = ByteString
type Base64String = ByteString

hexToBase64 :: HexString -> Base64String
hexToBase64 = BS64.encode . decodeHex

decodeHex :: HexString -> ByteString
decodeHex a | (decoded, "") <- BS16.decode a = decoded
            | otherwise = ByteString.concat ["invalid hex: ", a]

rawString :: ByteString -> String
rawString = Char8.unpack

xorBytes :: ByteString -> ByteString -> ByteString
xorBytes a = ByteString.pack . ByteString.zipWith xor a

bytesToHex :: ByteString -> HexString
bytesToHex = BS16.encode


hexToRaw :: HexString -> String
hexToRaw = rawString . decodeHex

hexToBytes :: HexString -> ByteString
hexToBytes = decodeHex

singleCharXOR :: Word8 -> ByteString -> ByteString
singleCharXOR c xs = ByteString.pack . ByteString.zipWith xor cs $ xs
                      where
                        len = fromIntegral . ByteString.length $ xs
                        cs = ByteString.pack . replicate len $ c

takeChunks :: Int -> ByteString -> [ByteString]
takeChunks len = ByteString.foldl' f []
  where
    f b a | null b = [ByteString.singleton a]
          | ByteString.length (last b) == fromIntegral len = b ++ [ByteString.singleton a]
          | otherwise = init b ++ [ByteString.append (last b) (ByteString.singleton a)]

trd :: (a, b, c) -> c
trd (_, _, c) = c
