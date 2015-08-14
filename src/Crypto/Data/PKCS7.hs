-- |
-- Module      : Crypto.Data.PKCS7
-- Copyright   : (c) K. Isom (2015)
-- License     : BSD-style
--
-- Maintainer  : coder@kyleisom.net
-- Stability   : stable
-- Portability : portable
--
-- This implements the PKCS #7 padding scheme for 'String's. This scheme is
-- defined in RFC 5652.
--
-- Strings will be padded out to multiples of the block size; for example,
-- a 5-byte string with an 8-byte block size will have three bytes of padding
-- added. If the length is already a multiple of the block size, an entire
-- block size worth of padding is added.
--
-- The padding bytes are all set to the number of padding bytes. Returning
-- to the previous example, the padding string would be three bytes of
-- the byte 0x03.
--
-- Unpadding checks the length and padding, and if this is valid, strips off
-- the padding.
module Crypto.Data.PKCS7 (
    pad, padN
  , unpad, unpadN
  , padBytes, padBytesN
  , unpadBytes, unpadBytesN
) where

import qualified Data.Char as C
import qualified Data.ByteString as B
import qualified Data.Word as W

padString :: Int -> String
padString n = replicate n (C.chr n)

-- | 'padN' applies the PKCS #7 padding scheme to the input string
-- given the 'blockSize'.
padN :: Int -> String -> String
padN blockSize s = s ++ padString padLength 
  where padLength = blockSize - remaining
        remaining = length s `mod` blockSize

-- | 'pad' applies the PKCS #7 padding scheme to the input string;
-- it uses the AES block size of 16 bytes as its block size.
pad :: String -> String
pad = padN 16

lengthCheck :: Int -> String -> Maybe String
lengthCheck blockSize s = if extra s /= 0
then Nothing
else Just s
  where extra = (`mod` blockSize) . length

validPadChar :: Int -> String -> Maybe String
validPadChar blockSize s = if C.ord (last s) <= blockSize
                              then Just s
                              else Nothing

dropPadding :: String -> Maybe String
dropPadding s = if all (== padChar) p
                   then Just m
                   else Nothing
  where padChar = last s
        leading = length s - C.ord padChar
        (m, p)  = splitAt leading s

-- | 'unpadN' attempts to remove the 'blockSize' padding from its input
-- 'String'. If the padding is invalid, 'Nothing' is returned.
unpadN :: Int -> String -> Maybe String
unpadN blockSize s = lengthCheck blockSize s
                 >>= validPadChar blockSize
                 >>= dropPadding

-- | 'unpad' attempts to remove the padding from the input 'String',
-- assuming an AES block size of 16 bytes.
unpad :: String -> Maybe String
unpad = unpadN 16


padByteString :: Int -> B.ByteString
padByteString n = B.pack . take n $ repeat padChar
  where padChar = fromIntegral n :: W.Word8

-- | 'padBytesN' applies the PKCS #7 padding scheme to the input bytestring
-- given the 'blockSize'.
padBytesN :: Int -> B.ByteString -> B.ByteString
padBytesN blockSize s = B.append s $ padByteString padLength
  where padLength = blockSize - remaining
        remaining = B.length s `mod` blockSize

-- | 'pad' applies the PKCS #7 padding scheme to the input string;
-- it uses the AES block size of 16 bytes as its block size.
padBytes :: B.ByteString -> B.ByteString
padBytes = padBytesN 16

lengthCheckBytes :: Int -> B.ByteString -> Maybe B.ByteString
lengthCheckBytes blockSize s = if extra s /= 0
    then Nothing
    else Just s
  where extra = (`mod` blockSize) . B.length

validPadCharBytes :: Int -> B.ByteString -> Maybe B.ByteString
validPadCharBytes blockSize s = if fromIntegral (B.last s) <= blockSize
  then Just s
  else Nothing

dropPaddingBytes :: B.ByteString -> Maybe B.ByteString
dropPaddingBytes s = if B.all (== fromIntegral padChar) p
    then Just m
    else Nothing
  where padChar = fromIntegral $ B.last s
        leading = B.length s - padChar
        (m, p)  = B.splitAt leading s

-- | 'unpadBytesN' attempts to remove the 'blockSize' padding from its input
-- 'ByteString'. If the padding is invalid, 'Nothing' is returned.
unpadBytesN :: Int -> B.ByteString -> Maybe B.ByteString
unpadBytesN blockSize s = lengthCheckBytes blockSize s
                 >>= validPadCharBytes blockSize
                 >>= dropPaddingBytes

-- | 'unpadBytes' attempts to remove the padding from the input 'ByteString',
-- assuming an AES block size of 16 bytes.
unpadBytes :: B.ByteString -> Maybe B.ByteString
unpadBytes = unpadBytesN 16
