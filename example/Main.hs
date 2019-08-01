{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS -Wall #-}

import Control.Monad
import Data.ByteString.Base16
import Data.ByteString.Char8  as Strict
import Data.ByteString.Short
import Data.IORef
import Data.Map.Strict
import Text.Printf

import DFINITY.RadixTree

type SimpleTree = RadixTree (IORef (Map ByteString ByteString))

-- |
-- Demonstrate the radix tree structure.
main :: IO ()
main = do

  -- Create an empty radix tree.
  ref  <- newIORef mempty
  tree <- createRadixTree 2048 Nothing ref
  printf "Empty\n"
  printRadixTree tree

  -- Insert key-value pairs into the radix tree.
  tree' <- foldM
    step
    tree
    [ ("cat"       , "dog")
    , ("cats"      , "ends in s")
    , ("catalog"   , "extends cat")
    , ("catalogs"  , "ends in s")
    , ("cataloging", "ends in ing")
    ]

  -- Merkleize the radix tree.
  (root, tree'') <- merkleizeRadixTree tree'
  printf "Merkleize\n"
  printRadixTree tree''
  let pretty = show . encode . fromShort
  printf "state root = %s\n" $ pretty root

-- |
-- Insert a key-value pair into the radix tree.
step :: SimpleTree -> (ByteString, ByteString) -> IO SimpleTree
step tree (key, value) = do
  printf "Insert %s %s\n" key' value'
  tree' <- insertRadixTree key value tree
  printRadixTree tree'
  return tree'
 where
  key'   = show key
  value' = show value
