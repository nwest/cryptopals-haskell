{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Set1 where

import Data.Word (Word8)
import qualified Data.ByteString             as BSStrict
import qualified Data.ByteString.Lazy.Char8  as Ch8
import qualified Data.ByteString.Lazy        as BS
import qualified Data.ByteString.Base16.Lazy as BS16
import qualified Data.ByteString.Base64.Lazy as BS64
import Data.Bits ((.|.), xor)

newtype ErrorString a = ErrorString {runError :: a} deriving (Show, Eq)
newtype HexString a = HexString {runHex :: a} deriving (Show, Eq)
newtype Base64String a = Base64String {runBase64 :: a} deriving (Show, Eq)

hexToBase64 :: HexString BS.ByteString -> Either (ErrorString BS.ByteString) (Base64String BS.ByteString)
hexToBase64 a = Base64String . BS64.encode <$> decodeHex a

decodeHex :: HexString BS.ByteString -> Either (ErrorString BS.ByteString) BS.ByteString
decodeHex a | (decoded, "") <- BS16.decode . runHex $ a = Right decoded
            | otherwise = Left . ErrorString $ BS.concat ["invalid hex: ", runHex a]
