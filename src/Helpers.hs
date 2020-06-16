
module Helpers where

import           Data.Bits (xor)
import qualified Data.ByteString as ByteString (zipWith, pack, null, splitAt)
import           Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as Char8 (unpack, replicate, length)

import qualified Data.ByteString.Base16 as BS16
import qualified Data.ByteString.Base64 as BS64
import Data.List (unfoldr)

type HexString = ByteString
type Base64String = ByteString

hexToBase64 :: HexString -> Base64String
hexToBase64 = BS64.encode . decodeHex

decodeHex :: HexString -> ByteString
decodeHex = fst . BS16.decode

xorBytes :: ByteString -> ByteString -> ByteString
xorBytes a = ByteString.pack . ByteString.zipWith xor a

bytesToHex :: ByteString -> HexString
bytesToHex = BS16.encode

hexToRaw :: HexString -> String
hexToRaw = Char8.unpack . decodeHex

hexToBytes :: HexString -> ByteString
hexToBytes = decodeHex

singleCharXOR :: Char -> ByteString -> ByteString
singleCharXOR c xs = ByteString.pack . ByteString.zipWith xor xs . Char8.replicate (Char8.length xs) $ c

takeChunks :: Int -> ByteString -> [ByteString]
takeChunks size = unfoldr (\bytes -> let (chunk, rest) = ByteString.splitAt size bytes
                                      in if ByteString.null chunk then Nothing
                                                                  else Just (chunk, rest))

fst3 :: (a, b, c) -> a
fst3 (a, _, _) = a

snd3 :: (a, b, c) -> b
snd3 (_, b, _) = b

trd :: (a, b, c) -> c
trd (_, _, c) = c

