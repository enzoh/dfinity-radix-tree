{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards       #-}

{-# OPTIONS -Wall #-}

module DFINITY.RadixTree.Types
  ( RadixBuffer
  , RadixCache
  , RadixDatabase(..)
  , RadixError(..)
  , RadixNode(..)
  , RadixPrefix(..)
  , RadixProof(..)
  , RadixRoot
  , RadixSearchResult
  , RadixTree(..)
  )
where

import Codec.Serialise          as CBOR
import Codec.Serialise.Decoding
import Codec.Serialise.Encoding
import Control.DeepSeq
import Control.Exception
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.ST
import Data.Bool
import Data.ByteString.Base16   as Base16
import Data.ByteString.Char8    as Strict
import Data.ByteString.Short
import Data.Data
import Data.Default.Class
import Data.IORef
import Data.List                as List
import Data.List.NonEmpty
import Data.LruCache
import Data.Map.Strict          as Map
import Data.Maybe
import Data.STRef
import Data.Word
import Database.LevelDB         as LevelDB
import Text.Printf

import DFINITY.RadixTree.Bits
import DFINITY.RadixTree.Serialise

type RadixBuffer = Map RadixRoot RadixNode

type RadixCache = LruCache RadixRoot RadixNode

class Monad m => RadixDatabase m database where
  load :: database -> ByteString -> m (Maybe ByteString)
  store :: database -> ByteString -> ByteString -> m ()

instance RadixDatabase (ST s) (STRef s (Map ByteString ByteString)) where
  load db key = Map.lookup key <$> readSTRef db
  store db key val = modifySTRef' db $ Map.insert key val

instance MonadIO m => RadixDatabase m (IORef (Map ByteString ByteString)) where
  load db key = liftIO $ Map.lookup key <$> readIORef db
  store db key val = liftIO $ modifyIORef' db $ Map.insert key val

instance MonadIO m => RadixDatabase m LevelDB.DB where
  load db = LevelDB.get db LevelDB.defaultReadOptions
  store db = LevelDB.put db LevelDB.defaultWriteOptions

data RadixError = InvalidArgument String | StateRootDoesNotExist RadixRoot
  deriving (Data, Eq, Show)

instance Exception RadixError

data RadixNode =
  RadixNode
  { _radixPrefix :: Maybe RadixPrefix
  , _radixLeft   :: Maybe RadixRoot
  , _radixRight  :: Maybe RadixRoot
  , _radixLeaf   :: Maybe ByteString
  } deriving (Data, Eq)

instance NFData RadixNode where
  rnf RadixNode {..} =
    rnf _radixPrefix `seq`
    rnf _radixLeft   `seq`
    rnf _radixRight  `seq`
    rnf _radixLeaf   `seq`
    ()

instance Default RadixNode where
  def = RadixNode Nothing Nothing Nothing Nothing

instance Serialise RadixNode where
  encode RadixNode {..} =
    encodeListLen len
      <> encodeMaybe CBOR.encode _radixPrefix
      <> encodeMaybe encodeSide left
      <> encodeMaybe encodeSide right
      <> maybe mempty encodeBytes _radixLeaf
    where
    len   = bool 3 4 $ isJust _radixLeaf
    left  = fromShort <$> _radixLeft
    right = fromShort <$> _radixRight
  decode = do
    len          <- decodeListLen
    _radixPrefix <- decodeMaybe CBOR.decode
    _radixLeft   <- decodeMaybe $ toShort <$> decodeSide
    _radixRight  <- decodeMaybe $ toShort <$> decodeSide
    _radixLeaf   <- decodeLeaf len
    pure $ RadixNode {..}

instance Show RadixNode where
  show RadixNode {..} =
    case show <$> _radixLeaf of
      Nothing   -> printf "@[\ESC[97m%s\ESC[0m,\ESC[96m%s\ESC[0m,\ESC[96m%s\ESC[0m]" prefix left right
      Just leaf -> printf "@[\ESC[97m%s\ESC[0m,\ESC[96m%s\ESC[0m,\ESC[96m%s\ESC[0m,\ESC[97m%s\ESC[0m]" prefix left right leaf
    where
    prefix  = maybe "null" show                   _radixPrefix
    left    = maybe "null" pretty $ fromShort <$> _radixLeft
    right   = maybe "null" pretty $ fromShort <$> _radixRight
    pretty  = List.take 8 . Strict.unpack . Base16.encode

data RadixPrefix =
  RadixPrefix
  { _radixBitLen :: Int
  , _radixName   :: ShortByteString
  } deriving (Data, Eq)

instance Bitable RadixPrefix where
  toBits RadixPrefix {..} = List.take _radixBitLen $ toBits _radixName
  fromBits bits = RadixPrefix bitLen name
    where
    bitLen = List.length bits
    name = fromBits bits

instance NFData RadixPrefix where
  rnf RadixPrefix {..} =
    rnf _radixBitLen `seq`
    rnf _radixName   `seq`
    ()

instance Serialise RadixPrefix where
  encode RadixPrefix {..} =
    encodeListLen 2
      <> encodeInt _radixBitLen
      <> encodeShort _radixName
  decode = do
    void decodeListLen
    RadixPrefix <$> decodeInt <*> decodeShort

instance Show RadixPrefix where
  show = List.map compress . toBits
    where compress = bool '0' '1'

data RadixProof =
  RadixProof
  { _radixPath  :: NonEmpty RadixNode
  , _radixValue :: ByteString
  } deriving (Data, Eq, Show)

instance NFData RadixProof where
  rnf RadixProof {..} =
    rnf _radixPath  `seq`
    rnf _radixValue `seq`
    ()

instance Serialise RadixProof where
  encode RadixProof {..} =
    CBOR.encode path
    where
    RadixNode {..} :| nodes = _radixPath
    node = RadixNode _radixPrefix _radixLeft _radixRight $ Just _radixValue
    path = node :| nodes
  decode = do
    RadixNode {..} :| nodes <- CBOR.decode
    case _radixLeaf of
      Nothing -> fail "no proof value"
      Just value -> do
        let node = RadixNode _radixPrefix _radixLeft _radixRight Nothing
        let path = node :| nodes
        pure $ RadixProof path value

type RadixRoot = ShortByteString

type RadixSearchResult = (NonEmpty RadixRoot, NonEmpty RadixNode, NonEmpty [Bool], [Bool], [Bool], RadixCache)

data RadixTree database =
  RadixTree
  { _radixBuffer     :: RadixBuffer
  , _radixCache      :: RadixCache
  , _radixCacheSize  :: Int
  , _radixCheckpoint :: RadixRoot
  , _radixDatabase   :: database
  , _radixNonce      :: Word32
  , _radixRoot       :: RadixRoot
  }
