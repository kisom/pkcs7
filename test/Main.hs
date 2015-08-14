{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Distribution.Simple.Test

import qualified Crypto.Data.PKCS7            as PKCS7
import qualified Data.ByteString              as B
import qualified Data.Char                    as C
import           Control.Applicative
import           Control.Monad
import           Data.Maybe
import           Data.Word
import qualified System.Exit                  as Exit
import           Test.QuickCheck
import           Test.HUnit

instance Arbitrary B.ByteString where
  arbitrary = liftM B.pack (listOf $ elements xs)
    where xs = map fromIntegral [1..255] :: [Word8]

prop_StringIdentity :: String -> Bool
prop_StringIdentity s = isJust (PKCS7.unpad (PKCS7.pad s))

prop_ByteStringIdentity :: B.ByteString -> Bool
prop_ByteStringIdentity s = isJust (PKCS7.unpadBytes (PKCS7.padBytes s))

padString n = if n > 1
              then [C.chr x | x <- [1..n]]
              else [C.chr (255 - x) | x <- [1..n]]

badPadding :: String -> String
badPadding s = s ++ padString padLength
  where blockSize = 16
        padLength = blockSize - remaining
        remaining = length s `mod` blockSize

prop_BadPadding1 :: String -> Bool
prop_BadPadding1 s = isNothing (PKCS7.unpad (badPadding s))

prop_BadPadding2 :: String -> Bool
prop_BadPadding2 s = isNothing (PKCS7.unpad $ tail (PKCS7.pad (badPadding s)))

quickCheckArgs = stdArgs { chatty = True }

checkResults r@(Failure{}) = do
  print r
  Exit.exitFailure
checkResults r@(NoExpectedFailure{}) = do
  print r
  Exit.exitFailure
checkResults r = return r

runQuickCheckProp prop = do
  result <- quickCheckWithResult quickCheckArgs prop
  checkResults result 
  return ()

runQuickCheck = do
  runQuickCheckProp prop_StringIdentity
  runQuickCheckProp prop_ByteStringIdentity
  runQuickCheckProp prop_BadPadding1
  runQuickCheckProp prop_BadPadding2 

main = runQuickCheck
