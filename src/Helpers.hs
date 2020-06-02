{-# LANGUAGE OverloadedStrings #-}

module Helpers where

import Data.Word (Word8)
import qualified Data.ByteString.Lazy        as BS
import qualified Data.ByteString.Base16.Lazy as BS16
import qualified Data.ByteString.Base64.Lazy as BS64
import Data.ByteString.Internal (w2c)
import Data.Bits (xor)
import Data.Either (fromRight)

type ErrorString = BS.ByteString
type HexString = BS.ByteString
type Base64String = BS.ByteString

hexToBase64 :: HexString -> Either ErrorString Base64String
hexToBase64 a = BS64.encode <$> decodeHex a

decodeHex :: HexString -> Either ErrorString BS.ByteString
decodeHex a | (decoded, "") <- BS16.decode a = Right decoded
            | otherwise = Left $ BS.concat ["invalid hex: ", a]

rawString :: BS.ByteString -> String
rawString = map w2c . BS.unpack

xorBytes :: BS.ByteString -> BS.ByteString -> Either ErrorString BS.ByteString
xorBytes a b | BS.length a == BS.length b = Right . BS.pack $ BS.zipWith xor a b
             | otherwise = Left "Mismatched lengths for XOR"

bytesToHex :: BS.ByteString -> HexString
bytesToHex = BS16.encode


hexToRaw :: HexString -> String
hexToRaw = rawString . fromRight "" <$> decodeHex

hexToBytes :: HexString -> BS.ByteString
hexToBytes = fromRight BS.empty . decodeHex

singleCharXOR :: Word8 -> BS.ByteString -> BS.ByteString
singleCharXOR c xs = BS.pack . BS.zipWith xor cs $ xs
                      where
                        len = fromIntegral . BS.length $ xs
                        cs = BS.pack . replicate len $ c
