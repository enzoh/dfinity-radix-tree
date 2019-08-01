{-# OPTIONS -Wall #-}

module DFINITY.RadixTree.Memory
  ( loadHot
  , loadCold
  , storeCold
  )
where

import Codec.Serialise
import Control.Monad.Trans.Resource
import Data.ByteArray
import Data.ByteString.Lazy
import Data.ByteString.Short
import Data.LruCache                as LRU
import Data.Map.Strict              as Map
import Database.LevelDB

import DFINITY.RadixTree.Hash
import DFINITY.RadixTree.Types

--------------------------------------------------------------------------------

loadHot
  :: RadixDatabase m database
  => RadixRoot
  -> RadixBuffer
  -> RadixCache
  -> database
  -> m (Maybe (RadixNode, RadixCache))
loadHot root buffer cache database = case Map.lookup root buffer of
  Just node -> pure $ Just (node, cache)
  Nothing   -> loadCold root cache database

{-# SPECIALISE loadHot
  :: RadixRoot
  -> RadixBuffer
  -> RadixCache
  -> DB
  -> ResourceT IO (Maybe (RadixNode, RadixCache)) #-}

--------------------------------------------------------------------------------

loadCold
  :: RadixDatabase m database
  => RadixRoot
  -> RadixCache
  -> database
  -> m (Maybe (RadixNode, RadixCache))
loadCold root cache database = case LRU.lookup root cache of
  Just (node, cache') -> seq cache' $ seq node $ pure $ Just (node, cache')
  Nothing             -> do
    let key = fromShort root
    result <- load database key
    case result of
      Just bytes -> do
        let node   = deserialise $ fromStrict bytes
        let cache' = LRU.insert root node cache
        seq cache' $ seq node $ pure $ Just (node, cache')
      Nothing -> pure Nothing

{-# SPECIALISE loadCold
  :: RadixRoot
  -> RadixCache
  -> DB
  -> ResourceT IO (Maybe (RadixNode, RadixCache)) #-}

--------------------------------------------------------------------------------

storeCold
  :: RadixDatabase m database
  => RadixNode
  -> RadixCache
  -> database
  -> m (RadixRoot, RadixCache)
storeCold node cache database = do
  store database key bytes
  seq cache' $ pure (root, cache')
 where
  bytes  = toStrict $ serialise node
  key    = convert $ blake2s160 bytes
  root   = toShort key
  cache' = LRU.insert root node cache

{-# SPECIALISE storeCold
  :: RadixNode
  -> RadixCache
  -> DB
  -> ResourceT IO (RadixRoot, RadixCache) #-}
