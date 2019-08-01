{-# OPTIONS -Wall #-}

module DFINITY.RadixTree.Utilities
  ( createParents
  , createPrefix
  , createRoot
  , createRootFromNonce
  , defaultRoot
  , ignoreIOErrors
  )
where

import Codec.Serialise
import Control.Exception
import Data.ByteArray
import Data.ByteString.Builder
import Data.ByteString.Lazy
import Data.ByteString.Short
import Data.Default.Class
import Data.List               as List
import Data.Word

import DFINITY.RadixTree.Bits
import DFINITY.RadixTree.Hash
import DFINITY.RadixTree.Types

createParents
  :: [RadixRoot]
  -> [RadixNode]
  -> [Bool]
  -> [[Bool]]
  -> [(RadixRoot, RadixNode, Bool)]
createParents roots nodes prefix =
  zip3 roots nodes . List.map List.head . List.init . (:) prefix

createPrefix :: [Bool] -> Maybe RadixPrefix
createPrefix bits = if List.null bits then Nothing else Just $ fromBits bits

createRoot :: RadixNode -> RadixRoot
createRoot = toShort . convert . blake2s160 . toStrict . serialise

createRootFromNonce :: Word32 -> RadixRoot
createRootFromNonce = toShort . toStrict . toLazyByteString . word32BE

defaultRoot :: RadixRoot
defaultRoot = createRoot def

ignoreIOErrors :: IO () -> IO ()
ignoreIOErrors = handle handler
  where handler = const $ return () :: IOError -> IO ()
