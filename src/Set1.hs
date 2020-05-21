{-# LANGUAGE OverloadedStrings #-}

module Set1 where

import Data.Word (Word8)
import qualified Data.ByteString.Lazy        as BS
import qualified Data.ByteString.Base16.Lazy as BS16
import qualified Data.ByteString.Base64.Lazy as BS64
import Data.ByteString.Internal (c2w, w2c)
import Data.Bits (xor)
import Data.List (foldl', sortOn)

newtype ErrorString a = ErrorString {runError :: a} deriving (Show, Eq)
newtype HexString a = HexString {runHex :: a} deriving (Show, Eq)
newtype Base64String a = Base64String {runBase64 :: a} deriving (Show, Eq)

hexToBase64 :: HexString BS.ByteString -> Either (ErrorString BS.ByteString) (Base64String BS.ByteString)
hexToBase64 a = Base64String . BS64.encode <$> decodeHex a

decodeHex :: HexString BS.ByteString -> Either (ErrorString BS.ByteString) BS.ByteString
decodeHex a | (decoded, "") <- BS16.decode . runHex $ a = Right decoded
            | otherwise = Left . ErrorString $ BS.concat ["invalid hex: ", runHex a]

rawString :: BS.ByteString -> String
rawString = map w2c . BS.unpack

xorBytes :: BS.ByteString -> BS.ByteString -> Either (ErrorString BS.ByteString) BS.ByteString
xorBytes a b | BS.length a == BS.length b = Right . BS.pack $ BS.zipWith xor a b
             | otherwise = Left . ErrorString $ "Mismatched lengths for XOR"

bytesToHex :: BS.ByteString -> HexString BS.ByteString
bytesToHex = HexString . BS16.encode


hexToRaw :: HexString BS.ByteString -> String
hexToRaw hs = let bs = decodeHex hs
               in case bs of
                    Right a -> rawString a
                    Left  b -> rawString . runError $ b

hexToBytes :: HexString BS.ByteString -> BS.ByteString
hexToBytes hs = let bs = decodeHex hs
                 in case bs of
                    Right a -> a
                    Left  b -> runError b

singleCharXOR :: Word8 -> BS.ByteString -> BS.ByteString
singleCharXOR c xs = BS.pack . BS.zipWith xor cs $ xs
                      where
                        len = fromIntegral . BS.length $ xs
                        cs = BS.pack . replicate len $ c

singleChars :: [Word8]
singleChars = map c2w ['A'..'z'] -- "ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_`abcdefghijklmnopqrstuvwxyz"

charEnglishScore :: Word8 -> Int
charEnglishScore c | f 'e' = 200
                   | f 't' = 180
                   | f 'a' = 160
                   | f 'o' = 140
                   | f 'l' = 120
                   | f 'i' = 100
                   | f 'n' = 80
                   | otherwise = -2
                  where
                    f x = c == c2w x


englishScore :: BS.ByteString -> Int
englishScore = foldl' (+) 0 . map charEnglishScore . BS.unpack

input3 :: BS.ByteString
input3 = hexToBytes . HexString $ "1b37373331363f78151b7f2b783431333d78397828372d363c78373e783a393b3736"

num3 :: BS.ByteString
num3 = let (_, _, x) = bestSingleCharXOR input3
        in x

bestSingleCharXOR :: BS.ByteString -> (Word8, Int, BS.ByteString)
bestSingleCharXOR xs = let scores = map f singleChars
                        in last . sortOn (\(_, a, _) -> a) $ scores
                       where f c = let xord = singleCharXOR c input3
                                       score = englishScore xord
                                    in (c, score, xord)

