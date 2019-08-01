{-# LANGUAGE ExplicitForAll #-}

{-# OPTIONS -Wall #-}

module DFINITY.RadixTree.Bits
  ( Bitable(..)
  , matchBits
  )
where

import Data.Bits
import Data.Bool
import Data.ByteString       as Strict
import Data.ByteString.Short as Short
import Data.List             as List
import Data.Word

class Bitable a where
  fromBits :: [Bool] -> a
  toBits :: a -> [Bool]

instance Bitable ByteString where
  fromBits = Strict.pack . go where
    go [] = []
    go xs = let (a, b) = List.splitAt 8 xs in toByte a : go b
  toBits = List.concatMap fromByte . Strict.unpack

instance Bitable ShortByteString where
  fromBits = Short.pack . go where
    go [] = []
    go xs = let (a, b) = List.splitAt 8 xs in toByte a : go b
  toBits = List.concatMap fromByte . Short.unpack

matchBits :: [Bool] -> [Bool] -> [Bool]
matchBits x = List.map fst . zipWhile (==) x

zipWhile :: (a -> b -> Bool) -> [a] -> [b] -> [(a, b)]
zipWhile f x = List.takeWhile (uncurry f) . List.zip x

fromByte :: Word8 -> [Bool]
fromByte = flip List.map order . testBit

toByte :: [Bool] -> Word8
toByte = List.foldl (.|.) 0 . List.zipWith (bool 0 . setBit 0) order

order :: Enum a => Num a => [a]
order = [7, 6 .. 0]
