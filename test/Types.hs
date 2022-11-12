{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}

{-# OPTIONS -Wall #-}

module Types
  ( Op(..)
  , MapDB(..)
  , PureMapDB
  , IOMapDB
  )
where

import Control.Monad
import Control.Monad.Identity
import Control.Monad.IO.Class
import Control.Monad.State.Strict
import Data.Aeson
import Data.Aeson.KeyMap          as KeyMap
import Data.ByteString.Base16     as Base16
import Data.ByteString.Char8      as Strict
import Data.Map                   as Map
import Data.Text                  as Text
import Data.Text.Encoding

import DFINITY.RadixTree

data Op
  = Insert ByteString ByteString
  | Delete ByteString
  | Lookup ByteString (Maybe ByteString)
  | Merkleize (Maybe ByteString)

instance FromJSON Op where
  parseJSON = \ case
    Object obj -> maybe mzero pure $ parse obj
    _          -> mzero

instance Show Op where
  show = \ case
    Insert key value -> "Insert"    ++ pretty key ++ pretty value
    Delete key       -> "Delete"    ++ pretty key
    Lookup key value -> "Lookup"    ++ pretty key ++ maybe " null" pretty value
    Merkleize  value -> "Merkleize" ++ maybe " null" pretty value
    where pretty = mappend " 0x" . Strict.unpack . Base16.encode

parse :: Object -> Maybe Op
parse obj = do
  op <- KeyMap.lookup "op" obj
  case op of
    "set" -> do
      key <- view obj "key"
      val <- view obj "value"
      pure $ Insert key val
    "delete" -> do
      key <- view obj "key"
      pure $ Delete key
    "get" -> do
      key <- view obj "key"
      let val = view obj "value"
      pure $ Lookup key val
    "stateRoot" -> do
      let val = view obj "value"
      pure $ Merkleize val
    _ -> Nothing

view :: Object -> Key -> Maybe ByteString
view obj key = do
  val <- KeyMap.lookup key obj
  case val of
    String text -> Just $ either error id $ Base16.decode $ encodeUtf8 $ Text.drop 2 text
    _           -> Nothing

newtype MapDB m a
  = MapDB { unMapDB :: StateT (Map ByteString ByteString) m a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadTrans)

instance Monad m => RadixDatabase (MapDB m) () where
  load  _ key     = MapDB $ Map.lookup key <$> get
  store _ key val = MapDB $ modify $ Map.insert key val

type PureMapDB = MapDB Identity
type IOMapDB = MapDB IO
