{-# LANGUAGE OverloadedStrings #-}

module Set2 where

import           Data.Char (isDigit, isLetter)
import           Codec.Crypto.SimpleAES (Key)
import qualified Data.ByteString.Lazy as Lazy (cycle, take, toStrict)
import qualified Data.ByteString as ByteString (drop, readFile, append, take, length)
import           Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as Char8 (pack, filter, unpack)
import qualified Data.ByteString.Base64 as Base64 (decodeLenient)
import           Data.List (find)
import qualified Data.Map.Strict as M (fromList, Map, lookup)
import           Text.ParserCombinators.ReadP
import qualified Data.Set as S

import           NWOpenSSL
import           Helpers

num9 :: ByteString
num9 = pkcs7pad 20 "YELLOW SUBMARINE"

num10 :: IO ByteString
num10 = do
  bytes <- Base64.decodeLenient . Char8.filter (/= '\n') <$> ByteString.readFile "./10.txt"
  return . decryptCBC zeroIV (Char8.pack "YELLOW SUBMARINE") $ bytes

num11 :: IO String
num11 = do
  boxBytes <- encryptionOracle . Lazy.toStrict $ (Lazy.take (10 * 16) . Lazy.cycle $ "YELLOW SUBMARINE")
  let variations = map (\i -> takeChunks 16 . ByteString.drop i $ boxBytes) [3..12]
      sizes = map (\bs -> (length bs, S.size . S.fromList $ bs)) variations
      smaller = filter (\(a, b) -> b < a) sizes
  if null smaller then return "It's probably CBC"
                  else return "It's probably ECB"


num12Bytes :: ByteString
num12Bytes = Base64.decodeLenient "Um9sbGluJyBpbiBteSA1LjAKV2l0aCBteSByYWctdG9wIGRvd24gc28gbXkgaGFpciBjYW4gYmxvdwpUaGUgZ2lybGllcyBvbiBzdGFuZGJ5IHdhdmluZyBqdXN0IHRvIHNheSBoaQpEaWQgeW91IHN0b3A/IE5vLCBJIGp1c3QgZHJvdmUgYnkK"

num12Encryptor :: Key -> Encryptor
num12Encryptor key bs = encryptECB key $ ByteString.append bs num12Bytes

num12 :: IO ()
num12 = do
  key <- randomAESKey
  let encryptor = num12Encryptor key
      keySize = ecbKeySize encryptor
  print keySize
  print $ isECB encryptor keySize
  print $ breakECB encryptor keySize

data UserProfile = UserProfile { email :: String, uid :: Int, role :: String } deriving (Show)

emailChar :: ReadP Char
emailChar = satisfy (\c -> isLetter c || isDigit c || (c `elem` ("_-@." :: String)))

parseKV :: ReadP (String, String)
parseKV = do
  key <- many $ satisfy isLetter
  _ <- string "="
  value <- many emailChar
  return (key, value)

parseCookie :: ReadP (M.Map String String)
parseCookie = M.fromList <$> sepBy1 parseKV (char '&')

profileFor :: String -> String
profileFor s = serializeProfile $ UserProfile (cleanse s) 10 "user"

cleanse :: String -> String
cleanse = filter (\c -> c `notElem` ("&=" :: String))

serializeProfile :: UserProfile -> String
serializeProfile (UserProfile email' uid' role') = 
  concat ["email=", email', "&uid=", show uid', "&role=", role']

deserializeProfile :: M.Map String String -> UserProfile
deserializeProfile m = let Just email' = M.lookup "email" m
                           Just uid' = M.lookup "uid" m
                           Just role' = M.lookup "role" m
                        in UserProfile email' (read uid') role'

num13 :: IO ()
num13 = do
  key <- randomAESKey
  let enc = encryptECB key . Char8.pack . profileFor . Char8.unpack
      dec = deserializeProfile . parse . Char8.unpack . decryptECB key
      prefix = replicate (16 - ecbPrefixLength enc 16) 'a'

      genBlock = secondChunk . enc . Char8.pack . (++) prefix
      adminChunk = genBlock "admin\0\0\0\0\0\0\0\0\0\0\0"
      userChunk = genBlock "user\0\0\0\0\0\0\0\0\0\0\0\0"
      
  let wedges = map (enc . Char8.pack . (++) prefix . flip replicate 'A') [1..]
      Just wedge = find (\bs -> let size = ByteString.length bs
                                    end = ByteString.drop (size - 16) bs
                                 in end == userChunk) wedges
  
  let noRole = ByteString.take (ByteString.length wedge - 16) wedge
      admin = ByteString.append noRole adminChunk
  print $ dec admin
      where
        parse = fst . last . readP_to_S parseCookie
        secondChunk = ByteString.take 16 . ByteString.drop 16

num14Encryptor :: Key -> ByteString -> Encryptor
num14Encryptor key pre bs = encryptECB key $ ByteString.append pre (ByteString.append bs num12Bytes)

num14 :: IO ()
num14 = do
  key <- randomAESKey
  pre <- randomPre
  let encryptor = num14Encryptor key pre
      keySize = ecbKeySize encryptor
  print $ breakECB encryptor keySize



