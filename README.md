# dfinity-radix-tree: A generic data integrity layer.

[![DFINITY][dfinity-shield]][dfinity]
[![Hackage][hackage-shield]][hackage]
[![Dependencies][deps-shield]][deps]
[![License: BSD 3-Clause][license-shield]][license]

## Overview

This library allows you to construct a [Merkle tree][wiki-merkle-tree] on top of
any underlying key-value database. It works by organizing your key-value pairs
into a binary [radix tree][wiki-radix-tree], which is well suited for storing
large dictionaries of fairly random keys, and is optimized for storing keys of
the same length.

## Usage

Define your database as an instance of the [`RadixDatabase`][] type class. An
instance for [LevelDB][hackage-leveldb-haskell] is already provided.

```haskell
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

import Control.Monad.IO.Class (MonadIO)
import Database.LevelDB (DB, defaultReadOptions, defaultWriteOptions, get, put)

import DFINITY.RadixTree

instance MonadIO m => RadixDatabase m DB where
  load database = get database defaultReadOptions
  store database = put database defaultWriteOptions
```

Create a [`RadixTree`][] that is parameterized by your database. If you want to
make things more explicit, then you can define a simple type alias and wrapper
function.

```haskell
import Control.Monad.Trans.Resource (MonadResource)
import Database.LevelDB (DB, Options(..), defaultOptions, open)

import DFINITY.RadixTree

type RadixTree' = RadixTree DB

createRadixTree'
  :: MonadResource m
  => FilePath -- Database.
  -> Maybe RadixRoot -- State root.
  -> m RadixTree'
createRadixTree' file root = do
  handle <- open file options
  createRadixTree cacheSize root handle
  where
  cacheSize = 2048
  options   = defaultOptions { createIfMissing = True }
```

Using the definitions above, you can create a radix tree, perform some basic
operations on it, and see that its contents is uniquely defined by its
[`RadixRoot`][].

```haskell
{-# LANGUAGE OverloadedStrings #-}

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Resource (runResourceT)
import Data.ByteString.Base16 (encode)
import Data.ByteString.Char8 (unpack)
import Data.ByteString.Short (fromShort)

import DFINITY.RadixTree

main :: IO ()
main = runResourceT $ do

  -- Create a radix tree, insert a key-value pair, and Merkleize.
  tree  <- createRadixTree' "/path/to/database" Nothing
  tree' <- insertRadixTree "Hello" "World" tree
  root  <- fst <$> merkleizeRadixTree tree'

  -- Print the state root.
  liftIO $ putStrLn $ "State Root: 0x" ++ pretty root
  where pretty = unpack . encode . fromShort
```

Running the program above should produce the following result.

```
State Root: 0xb638755216858bc84de8b80f480f15ca5c733e95
```

## License

`dfinity-radix-tree` is licensed under the [BSD 3-Clause License][license].

[//]: # (----------------------------------------------------------------------)

[dfinity]:
  https://dfinity.org
[dfinity-shield]:
  https://img.shields.io/badge/made%20by-DFINITY-29abe2.svg
[hackage]:
  https://hackage.haskell.org/package/dfinity-radix-tree
[hackage-shield]:
  https://img.shields.io/hackage/v/dfinity-radix-tree.svg
[deps]:
  https://packdeps.haskellers.com/feed?needle=dfinity-radix-tree
[deps-shield]:
  https://img.shields.io/hackage-deps/v/dfinity-radix-tree.svg
[license]:
  https://en.wikipedia.org/wiki/BSD_licenses
[license-shield]:
  https://img.shields.io/badge/license-BSD%203--Clause-29abe2.svg
[wiki-radix-tree]:
  https://en.wikipedia.org/wiki/Radix_tree
[wiki-merkle-tree]:
  https://en.wikipedia.org/wiki/Merkle_tree
[hackage-leveldb-haskell]:
  https://hackage.haskell.org/package/leveldb-haskell
[`RadixDatabase`]:
  https://hackage.haskell.org/package/dfinity-radix-tree/docs/DFINITY-RadixTree.html#t:RadixDatabase
[`RadixTree`]:
  https://hackage.haskell.org/package/dfinity-radix-tree/docs/DFINITY-RadixTree.html#t:RadixTree
[`RadixRoot`]:
  https://hackage.haskell.org/package/dfinity-radix-tree/docs/DFINITY-RadixTree.html#t:RadixRoot

[//]: # (----------------------------------------------------------------------)
