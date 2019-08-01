{-# OPTIONS -Wall #-}

module DFINITY.RadixTree.Lock
  ( withReadLock
  , withWriteLock
  )
where

import Control.Concurrent.ReadWriteLock
import Control.Monad.Trans.Resource

withLock
  :: MonadResource m
  => (RWLock -> IO ())
  -> (RWLock -> IO ())
  -> RWLock
  -> m a
  -> m a
withLock f g lock action = do
  key    <- fst <$> allocate acquireLock releaseLock
  result <- action
  release key
  pure result
 where
  acquireLock = f lock
  releaseLock = const $ g lock

withReadLock :: MonadResource m => RWLock -> m a -> m a
withReadLock = withLock acquireRead releaseRead

withWriteLock :: MonadResource m => RWLock -> m a -> m a
withWriteLock = withLock acquireWrite releaseWrite
