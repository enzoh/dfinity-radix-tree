{-# LANGUAGE ExplicitForAll   #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE TypeApplications #-}

{-# OPTIONS -Wall #-}

module Integrations
  ( tests
  )
where

import Control.Arrow
import Control.Concurrent.BoundedChan
import Control.Concurrent.MVar
import Control.Concurrent.ReadWriteLock
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Resource
import Crypto.Hash
import Data.ByteArray
import Data.ByteString.Base16
import Data.ByteString.Builder
import Data.ByteString.Char8            as Strict
import Data.ByteString.Lazy
import Data.ByteString.Short
import Data.Conduit                     as Conduit
import Data.Conduit.Internal
import Data.List                        as List
import Data.Word
import Database.LevelDB
import System.Clock
import System.FilePath
import System.IO.Temp
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import Text.Printf

import DFINITY.RadixTree
import DFINITY.RadixTree.Conduit

tests :: TestTree
tests = testGroup
  "integrations"
  [ testCase "simple-01-25000" $ simple 01 25000
  , testCase "simple-01-50000" $ simple 01 50000
  , testCase "simple-02-25000" $ simple 02 25000
  , testCase "simple-02-50000" $ simple 02 50000
  , testCase "simple-04-25000" $ simple 04 25000
  , testCase "simple-04-50000" $ simple 04 50000
  , testCase "simple-08-25000" $ simple 08 25000
  , testCase "simple-08-50000" $ simple 08 50000
  , testCase "simple-16-25000" $ simple 16 25000
  , testCase "simple-16-50000" $ simple 16 50000
  ]

simple :: Int -> Word32 -> Assertion
simple n size = withSystemTempDirectory "test" $ \path -> do
  -- Create concurrent data structures.
  counter    <- newMVar 0
  senders    <- replicateM n $ newBoundedChan 64
  receiver   <- newBoundedChan 64
  -- Create the source and target database locks.
  sourceLock <- new
  targetLock <- new
  -- Run the deterministic resource allocator.
  runResourceT $ do
    -- Create the source and target trees.
    sourceTree  <- createRadixTree' path "source"
    targetTree  <- createRadixTree' path "target"
    -- Saturate the source tree.
    sourceTree' <- saturate 1 size sourceTree
    -- Calculate the source tree state root.
    sourceRoot' <- fst <$> merkleizeRadixTree sourceTree'
    liftIO $ printf "\n      source: %s\n" $ pretty sourceRoot'
    -- Create thread to relay updates.
    void $ resourceForkIO $ liftIO $ forever $ do
      update <- readChan receiver
      forM_ senders $ flip writeChan update
    -- Define the state synchronization conduits.
    let bitmasks = genBitmasks n
    let zipper mask sender =
          sourceRadixTree mask 2048 sender sourceTree' sourceLock
    let source = merge $ List.zipWith zipper bitmasks senders
    let sink   = sinkRadixTree sourceRoot' receiver targetTree targetLock
    -- Run the state synchronization protocol.
    begin  <- liftIO $ getTime Monotonic
    result <- runConduit $ source .| bandwidth counter .| sink
    end    <- liftIO $ getTime Monotonic
    -- Inspect the result.
    case result of
      Left  _           -> fail "simple: missing subtrees"
      Right targetTree' -> do
        -- Calculate the target tree state root.
        targetRoot' <- fst <$> merkleizeRadixTree targetTree'
        liftIO $ printf "      target: %s\n" $ pretty targetRoot'
        -- Display bandwidth utilization.
        bytes <- liftIO $ readMVar counter
        let mbits   = 8 * realToFrac bytes / 1000000
        let nanos   = toNanoSecs end - toNanoSecs begin
        let seconds = realToFrac nanos / 1000000000
        let mbps    = mbits / seconds :: Double
        liftIO $ printf "      bandwidth: %.03f megabits per second\n" mbps
        -- Assert the source and target tree state roots as equal.
        liftIO $ assertEqual "simple" sourceRoot' targetRoot'

createRadixTree' :: MonadResource m => FilePath -> String -> m (RadixTree DB)
createRadixTree' path name = do
  handle <- open database options
  liftIO $ createRadixTree 2028 Nothing handle
 where
  database = path </> name
  options  = defaultOptions { createIfMissing = True }

saturate
  :: MonadIO m
  => RadixDatabase m database
  => Word32
  -> Word32
  -> RadixTree database
  -> m (RadixTree database)
saturate a b tree = foldM step tree [a .. b]
 where
  step accum x = do
    let key = hashW32 x
    value  <- liftIO $ generate $ Strict.pack <$> arbitrary
    accum' <- insertRadixTree key value accum
    if mod x 1000 == 0 then snd <$> merkleizeRadixTree accum' else pure accum'

merge :: Monad m => [ConduitT () o m ()] -> ConduitT () o m ()
merge = go . List.map sourceToPipe
 where
  go pipes = do
    pipes' <- foldM step [] pipes
    if List.null pipes' then pure () else go pipes'
  step accum = \case
    Done ()               -> pure accum
    HaveOutput next value -> do
      Conduit.yield value
      pure $ next : accum
    PipeM action -> do
      next <- lift action
      step accum next
    _ -> fail "merge: undefined"

bandwidth
  :: MonadIO m
  => MVar Word64
  -> ConduitT Strict.ByteString Strict.ByteString m ()
bandwidth counter = Conduit.awaitForever $ \bytes -> do
  let size = fromIntegral $ Strict.length bytes
  liftIO $ modifyMVar_ counter $ \accum -> pure $! accum + size
  Conduit.yield bytes

pretty :: RadixRoot -> String
pretty = Strict.unpack . encode . fromShort

hashW32 :: Word32 -> Strict.ByteString
hashW32 =
  convert
    . id @(Digest Blake2s_160)
    . hash
    . toStrict
    . toLazyByteString
    . word32BE

genBitmasks :: Int -> [[Bool]]
genBitmasks n = List.take n $ List.concat $ List.repeat xs
 where
  i = truncate $ logBase 2 $ (realToFrac n :: Double)
  xs =
    [ List.zipWith arr fs $ List.replicate i True
    | fs <- replicateM i [id, not]
    ]
