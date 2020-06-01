{-# LANGUAGE OverloadedStrings #-}

module Set1 where

import Data.Word (Word8)
import qualified Data.ByteString.Lazy        as BS
import qualified Data.ByteString             as B
import qualified Data.ByteString.Base16.Lazy as BS16
import qualified Data.ByteString.Base64.Lazy as BS64
import Data.ByteString.Internal (c2w, w2c)
import Data.Bits (xor, popCount)
import Data.List (foldl', sortOn)
import qualified Data.Set as S
import OpenSSL.EVP.Cipher
import OpenSSL.EVP.Base64

type ErrorString = BS.ByteString
newtype HexString a = HexString {runHex :: a} deriving (Show, Eq)
newtype Base64String a = Base64String {runBase64 :: a} deriving (Show, Eq)

hexToBase64 :: HexString BS.ByteString -> Either (ErrorString) (Base64String BS.ByteString)
hexToBase64 a = Base64String . BS64.encode <$> decodeHex a

decodeHex :: HexString BS.ByteString -> Either (ErrorString) BS.ByteString
decodeHex a | (decoded, "") <- BS16.decode . runHex $ a = Right decoded
            | otherwise = Left $ BS.concat ["invalid hex: ", runHex a]

rawString :: BS.ByteString -> String
rawString = map w2c . BS.unpack

xorBytes :: BS.ByteString -> BS.ByteString -> Either (ErrorString) BS.ByteString
xorBytes a b | BS.length a == BS.length b = Right . BS.pack $ BS.zipWith xor a b
             | otherwise = Left "Mismatched lengths for XOR"

bytesToHex :: BS.ByteString -> HexString BS.ByteString
bytesToHex = HexString . BS16.encode


hexToRaw :: HexString BS.ByteString -> String
hexToRaw hs = let bs = decodeHex hs
               in case bs of
                    Right a -> rawString a
                    Left  b -> rawString b

hexToBytes :: HexString BS.ByteString -> BS.ByteString
hexToBytes hs = let bs = decodeHex hs
                 in case bs of
                    Right a -> a
                    Left  b -> b

singleCharXOR :: Word8 -> BS.ByteString -> BS.ByteString
singleCharXOR c xs = BS.pack . BS.zipWith xor cs $ xs
                      where
                        len = fromIntegral . BS.length $ xs
                        cs = BS.pack . replicate len $ c

singleChars :: [Word8]
singleChars = [33..122] ++ [c2w ' ']

singleCharsNice :: String
singleCharsNice = map w2c singleChars

charEnglishScore :: Word8 -> Int
charEnglishScore c | f 'e' = 2200
                   | f 't' = 2000
                   | f 'a' = 1800
                   | f 'o' = 1200
                   | f 'i' = 1000
                   | f 'n' = 800
                   | f 's' = 800
                   | f 'h' = 800
                   | f 'r' = 800
                   | f 'd' = 800
                   | f 'u' = 800
                   | otherwise = -3000
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
                       where f c = let xord = singleCharXOR c xs
                                       score = englishScore xord
                                    in (c, score, xord)

readByteList :: [String] -> [BS.ByteString]
readByteList = map (hexToBytes . HexString . BS.pack . map c2w)

bslines :: BS.ByteString -> [BS.ByteString]
bslines = BS.split (c2w '\n')

num4 :: IO BS.ByteString
num4 = do
       input <- map (bestSingleCharXOR . hexToBytes . HexString) . bslines <$> BS.readFile "./4.txt"
       let (_, _, st) = last . sortOn (\(_, x, _ ) -> x) $ input
       return st

bsrepeat :: BS.ByteString -> BS.ByteString
bsrepeat bs = BS.append bs (bsrepeat bs)

repeatKeyXOR :: BS.ByteString -> BS.ByteString -> BS.ByteString
repeatKeyXOR k bs = let key = BS.take (BS.length bs) (bsrepeat k)
                        xord = xorBytes key bs
                     in case xord of
                          Right a -> a
                          Left b -> b

editDistance :: BS.ByteString -> BS.ByteString -> Int
editDistance a b = sum . zipWith edit (BS.unpack a) $ BS.unpack b
                   where
                     edit x0 x1 = popCount $ xor x0 x1

takeChunks :: Int -> BS.ByteString -> [BS.ByteString]
takeChunks len = BS.foldl f []
  where
    f b a | null b = [BS.singleton a]
          | BS.length (last b) == fromIntegral len = b ++ [BS.singleton a]
          | otherwise = init b ++ [BS.append (last b) (BS.singleton a)]

keySize :: BS.ByteString -> Int
keySize bs = let possible = map (take 10 . (`takeChunks` bs)) [2..40]
             in editScores possible

editScores :: [[BS.ByteString]] -> Int
editScores = fst . head . sortOn snd . map editScore

editScore :: [BS.ByteString] -> (Int, Float)
editScore [] = (0, 0)
editScore bs@(b:_) = let len = length bs
                         keyLength = fromIntegral . BS.length $ b
                         scores = zipWith editDistance bs (drop 1 bs)
                         avgScoreNormal = (fromIntegral (sum scores) / fromIntegral len) / fromIntegral keyLength
                      in (keyLength, avgScoreNormal) 

num6 :: IO BS.ByteString
num6 = do
       input <- BS64.decode . BS.filter (/= (c2w '\n')) <$> BS.readFile "./6.txt"
       let bs = case input of
                  Right bs' -> bs'
                  Left s -> BS.pack . map c2w $ s
           keyLen = keySize bs
           decrypted = BS.concat . BS.transpose . map (trd . bestSingleCharXOR) . BS.transpose $ takeChunks keyLen bs
       return decrypted

trd :: (a, b, c) -> c
trd (_, _, c) = c

ciphers :: IO [String]
ciphers = getCipherNames

num7 :: IO BS.ByteString
num7 = do cip       <- getCipherByName $ "aes-128-ecb"
          encrypted <- decodeBase64LBS . BS.filter (/= (c2w '\n')) <$> BS.readFile "./7.txt"
          let key = B.pack . map c2w $ "YELLOW SUBMARINE"
          case cip of
              Just c -> (cipherLBS c key B.empty Decrypt encrypted)
              Nothing ->  return BS.empty

num8 :: IO (S.Set BS.ByteString)
num8 = do 
        possible <- init . map (takeChunks 16 . hexToBytes . HexString) . BS.split (c2w '\n') <$> BS.readFile "./8.txt"
        let sets = map S.fromList possible
            smallest = head . sortOn S.size $ sets
        return smallest
