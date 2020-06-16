{-# LANGUAGE OverloadedStrings #-}

module Set1 where

import Helpers
import NWOpenSSL

import           Data.Bits (xor, popCount)
import qualified Data.ByteString as ByteString
import           Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8  as Char8 (unpack, pack, split, filter)
import qualified Data.ByteString.Base64 as BS64

import Data.Char (chr)

import Data.List (foldl', sortOn)
import qualified Data.Set as S

singleChars :: String
singleChars = map chr [32..122]

charEnglishScore :: Char -> Int
charEnglishScore 'e' = 2200
charEnglishScore 't' = 2000
charEnglishScore 'a' = 1800
charEnglishScore 'o' = 1200
charEnglishScore 'i' = 1000
charEnglishScore 'n' = 800
charEnglishScore 's' = 800
charEnglishScore 'h' = 800
charEnglishScore 'r' = 800
charEnglishScore 'd' = 800
charEnglishScore 'u' = 800
charEnglishScore  _  = -3000

englishScore :: ByteString -> Int
englishScore = foldl' (+) 0 . map charEnglishScore . Char8.unpack

input3 :: ByteString
input3 = hexToBytes "1b37373331363f78151b7f2b783431333d78397828372d363c78373e783a393b3736"

num3 :: ByteString
num3 = trd . bestSingleCharXOR $ input3

bestSingleCharXOR :: ByteString -> (Char, Int, ByteString)
bestSingleCharXOR xs = let scores = map f singleChars
                        in last . sortOn (\(_, a, _) -> a) $ scores
                       where f c = let xord = singleCharXOR c xs
                                       score = englishScore xord
                                    in (c, score, xord)

readByteList :: [String] -> [ByteString]
readByteList = map (hexToBytes . Char8.pack)

bslines :: ByteString -> [ByteString]
bslines = Char8.split '\n'
 
num4 :: IO ByteString
num4 = do
       input <- map (bestSingleCharXOR . hexToBytes) . bslines <$> ByteString.readFile "./4.txt"
       let (_, _, st) = last . sortOn (\(_, x, _ ) -> x) $ input
       return st

bsrepeat :: ByteString -> ByteString
bsrepeat bs = ByteString.append bs (bsrepeat bs)

repeatKeyXOR :: ByteString -> ByteString -> ByteString
repeatKeyXOR k bs = let key = ByteString.take (ByteString.length bs) (bsrepeat k)
                     in xorBytes key bs

editDistance :: ByteString -> ByteString -> Int
editDistance a b = sum . zipWith edit (ByteString.unpack a) $ ByteString.unpack b
                   where
                     edit x0 x1 = popCount $ xor x0 x1

keySize :: ByteString -> Int
keySize bs = editScores . map (take 10 . (`takeChunks` bs)) $ [2..40]

editScores :: [[ByteString]] -> Int
editScores = fst . head . sortOn snd . map editScore

editScore :: [ByteString] -> (Int, Float)
editScore [] = (0, 0)
editScore bs@(b:_) = let len = length bs
                         keyLength = fromIntegral . ByteString.length $ b
                         scores = zipWith editDistance bs (drop 1 bs)
                         avgScoreNormal = (fromIntegral (sum scores) / fromIntegral len) / fromIntegral keyLength
                      in (keyLength, avgScoreNormal) 

num6 :: IO ByteString
num6 = do
       input <- BS64.decodeLenient . Char8.filter (/= '\n') <$> ByteString.readFile "./6.txt"
       let keyLen = keySize input
           decrypted = ByteString.concat . ByteString.transpose . map (trd . bestSingleCharXOR) . ByteString.transpose $ takeChunks keyLen input
       return decrypted

num7 :: IO ByteString
num7 = do encrypted <- BS64.decodeLenient . Char8.filter (/= '\n') <$> ByteString.readFile "./7.txt"
          let key = Char8.pack "YELLOW SUBMARINE"
          return . decryptECB key $ encrypted

num8 :: IO (S.Set ByteString)
num8 = do 
        possible <- init . map (takeChunks 16 . hexToBytes) . Char8.split '\n' <$> ByteString.readFile "./8.txt"
        let sets = map S.fromList possible
            smallest = head . sortOn S.size $ sets
        return smallest
