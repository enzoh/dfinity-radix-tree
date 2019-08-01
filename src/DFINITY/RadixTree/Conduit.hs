{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE Rank2Types            #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}

{-# OPTIONS -Wall                        #-}
{-# OPTIONS -Wno-partial-type-signatures #-}

-- |
-- Module     : DFINITY.RadixTree.Conduit
-- Copyright  : 2018-2019 DFINITY Stiftung
-- License    : BSD-3-Clause
-- Maintainer : Enzo Haussecker <enzo@dfinity.org>
-- Stability  : Stable
--
-- A parallel download protocol.
module DFINITY.RadixTree.Conduit
  (

  -- ** Combinators
    sourceRadixTree
  , sinkRadixTree
  )
where

import Codec.Serialise
import Control.Concurrent
import Control.Concurrent.BoundedChan   as BChan
import Control.Concurrent.ReadWriteLock
import Control.Exception
import Control.Monad                    as Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Resource
import Data.ByteArray
import Data.ByteString                  as Strict
import Data.ByteString.Lazy
import Data.ByteString.Short
import Data.Conduit                     as Conduit
import Data.HashTable.IO                as Cuckoo
import Data.List                        as List
import Data.LruCache                    as LRU
import Data.Maybe
import Database.LevelDB
import Database.LevelDB.Base            as LevelDB
import Database.LevelDB.Internal
import System.Directory
import System.IO.Temp

import DFINITY.RadixTree.Bits
import DFINITY.RadixTree.Hash
import DFINITY.RadixTree.Lenses
import DFINITY.RadixTree.Lock
import DFINITY.RadixTree.Types
import DFINITY.RadixTree.Utilities

--------------------------------------------------------------------------------

-- |
-- Create a conduit from a radix tree.
sourceRadixTree
  :: forall m database
   . MonadResource m
  => RadixDatabase (ConduitT () Strict.ByteString m) database
  => [Bool]
  -- ^ Bitmask.
  -> Int
  -- ^ LRU cache size in items.
  -> BoundedChan RadixRoot
  -- ^ Terminal state root producer.
  -> RadixTree database
  -- ^ Radix tree.
  -> RWLock
  -- ^ Radix database lock.
  -> ConduitT () Strict.ByteString m ()
sourceRadixTree bitmask cacheSize chan tree radixLock
  | cacheSize <= 0 = throw $ InvalidArgument "invalid LRU cache size"
  | otherwise = do
    cache  <- liftIO $ newMVar $ LRU.empty cacheSize
    action <- fmap fst $ flip allocate killThread $ forkIO $ forever $ do
      root <- BChan.readChan chan
      modifyMVar_ cache $ pure . LRU.insert root ()
    loop cache tree []
    release action
 where

  loop
    :: MVar (LruCache RadixRoot ())
    -> RadixTree database
    -> [RadixRoot]
    -> ConduitT () Strict.ByteString m ()
  loop cache subtree@RadixTree {..} accum = do
    let accum' = _radixCheckpoint : accum
    seen <- liftIO $ readMVar cache
    if flip List.any accum' $ isJust . flip LRU.lookup seen
      then pure ()
      else do
        let key = fromShort _radixCheckpoint
        result <- withReadLock radixLock $ load _radixDatabase key
        case result of
          Nothing    -> pure ()
          Just bytes -> do
            let RadixNode {..} = deserialise $ fromStrict bytes
            let checkpoint     = toBits $ fromShort _radixCheckpoint
            let success        = and $ List.zipWith (==) bitmask checkpoint
            when success $ Conduit.yield bytes
            forM_ [_radixLeft, _radixRight] $ \case
              Nothing -> pure ()
              Just root ->
                loop cache `flip` accum' $ setCheckpoint root subtree

{-# SPECIALISE sourceRadixTree
               :: [Bool]
               -> Int
               -> BoundedChan RadixRoot
               -> RadixTree DB
               -> RWLock
               -> ConduitT () Strict.ByteString (ResourceT IO) () #-}

--------------------------------------------------------------------------------

-- |
-- Create a radix tree from a conduit.
sinkRadixTree
  :: forall m database
   . MonadResource m
  => RadixDatabase (ConduitT Strict.ByteString Void m) database
  => RadixRoot
  -- ^ Target state root.
  -> BoundedChan RadixRoot
  -- ^ Terminal state root consumer.
  -> RadixTree database
  -- ^ Radix tree.
  -> RWLock
  -- ^ Radix database lock.
  -> ConduitT
       Strict.ByteString
       Void
       m
       (Either String (RadixTree database))
sinkRadixTree checkpoint chan tree@RadixTree {..} radixLock = do
  -- Create a temporary directory.
  relative <- liftIO getTemporaryDirectory
  absolute <- liftIO $ canonicalizePath relative
  let createTempDir  = createTempDirectory absolute "dfinity"
  let destroyTempDir = ignoreIOErrors . removeDirectoryRecursive
  (tempDirKey, tempDir) <- allocate createTempDir destroyTempDir
  -- Create a temporary database.
  let options            = defaultOptions { createIfMissing = True }
  let createTempDatabase = LevelDB.open tempDir options
  (tempDatabaseKey, tempDatabase) <- allocate createTempDatabase unsafeClose
  -- Create a hash table.
  table                           <- liftIO $ fromList [(checkpoint, Nothing)]
  -- Consume the radix nodes.
  result                          <- loop1 tempDatabase table
  -- Remove the temporary database.
  release tempDatabaseKey
  release tempDirKey
  -- Return the result.
  pure result
 where

  -- Loop 1: The collection loop.
  loop1
    :: DB
    -> CuckooHashTable RadixRoot (Maybe RadixRoot)
    -> ConduitT
         Strict.ByteString
         Void
         m
         (Either String (RadixTree database))
  loop1 tempDatabase table = do
    -- Have we collected all the radix nodes?
    done <- liftIO $ isNothing <$> Cuckoo.lookup table checkpoint
    if done
      then pure $ Right $ setCheckpoint checkpoint $ setRoot checkpoint tree
      else do
        -- Wait for a radix node.
        mval <- await
        case mval of
          Nothing   -> pure $ Left "EOF"
          Just node -> case deserialiseOrFail $ fromStrict node of
            Left  _              -> loop1 tempDatabase table
            Right RadixNode {..} -> do
              -- Does the radix node already exist in the radix database?
              let key  = convert $ blake2s160 node
              let root = toShort key
              want   <- liftIO $ isJust <$> Cuckoo.lookup table root
              exists <- if want
                then pure False
                else withReadLock radixLock $ isJust <$> load _radixDatabase key
              if exists
                then do
                  -- Announce a terminal state root.
                  liftIO $ void $ tryWriteChan chan root
                  -- Discard the radix node.
                  liftIO $ Cuckoo.delete table root
                  loop1 tempDatabase table
                else do
                  -- Identify any children not present in the radix database.
                  let absent =
                        fmap isNothing
                          . withReadLock radixLock
                          . load _radixDatabase
                          . fromShort
                  let children =
                        maybe id (:) _radixLeft $ maybe id (:) _radixRight []
                  targets <- filterM absent children
                  -- Write the radix node and its targets to the temporary database.
                  let value = toStrict $ serialise (node, targets)
                  store tempDatabase key value
                  -- Does the radix node have any gaps in its lineage?
                  if not want
                    then loop1 tempDatabase table
                    else do
                      -- Write all eligible radix nodes to the radix database.
                      eligible <- loop2 tempDatabase table root []
                      loop3 tempDatabase table eligible
                      loop1 tempDatabase table

  -- Loop 2: The aggregation loop.
  loop2
    :: DB
    -> CuckooHashTable RadixRoot (Maybe RadixRoot)
    -> RadixRoot
    -> [(RadixRoot, Strict.ByteString)]
    -> ConduitT
         Strict.ByteString
         Void
         m
         [(RadixRoot, Strict.ByteString)]
  loop2 tempDatabase table root eligible = do
    -- Read the radix node and its targets from the temporary database.
    result <- load tempDatabase $ fromShort root
    case deserialise . fromStrict <$> result of
      Nothing                       -> pure eligible
      Just (bytes, targets :: [] _) -> do
        -- Is the radix node eligible to be written to disk?
        if List.null targets
          then pure $ (root, bytes) : eligible
          else do
            -- Write the child-parent pairs to the hash table.
            liftIO $ forM_ targets $ \child ->
              Cuckoo.insert table child $ Just root
            -- Recurse.
            Monad.foldM step eligible targets
        where step = flip $ loop2 tempDatabase table

  -- Loop 3: The write loop.
  loop3
    :: DB
    -> CuckooHashTable RadixRoot (Maybe RadixRoot)
    -> [(RadixRoot, Strict.ByteString)]
    -> ConduitT Strict.ByteString Void m ()
  loop3 tempDatabase table = \case
    []                       -> pure ()
    (root, bytes) : eligible -> do
      -- Write the radix node to the radix database.
      let key = fromShort root
      withWriteLock radixLock $ store _radixDatabase key bytes
      -- Delete the radix node from the temporary database.
      LevelDB.delete tempDatabase defaultWriteOptions $ fromShort root
      Just parent <- liftIO $ Cuckoo.lookup table root
      liftIO $ Cuckoo.delete table root
      case parent of
        Nothing    -> pure ()
        Just root' -> do
          let key' = fromShort root'
          load tempDatabase key' >>= \case
            Nothing    -> loop3 tempDatabase table eligible
            Just value -> do
              let (bytes', targets') = deserialise $ fromStrict value
              let targets''          = List.delete root targets'
              -- Have both siblings been written to the radix database?
              if List.null targets''
                then do
                  let eligible' = (root', bytes') : eligible
                  loop3 tempDatabase table eligible'
                else do
                  -- Update the parent node.
                  let value' = toStrict $ serialise (bytes', targets'')
                  store tempDatabase key' value'
                  loop3 tempDatabase table eligible

{-# SPECIALISE sinkRadixTree
               :: RadixRoot
               -> BoundedChan RadixRoot
               -> RadixTree DB
               -> RWLock
               -> ConduitT Strict.ByteString
                           Void
                           (ResourceT IO)
                           (Either String (RadixTree DB)) #-}
