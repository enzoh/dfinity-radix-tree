{-# OPTIONS -Wall #-}

module DFINITY.RadixTree.Hash
  ( blake2s160
  )
where

import Crypto.Hash
import Data.ByteString

blake2s160 :: ByteString -> Digest Blake2s_160
blake2s160 = hash
