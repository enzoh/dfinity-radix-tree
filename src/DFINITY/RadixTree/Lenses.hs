{-# LANGUAGE NamedFieldPuns #-}

{-# OPTIONS -Wall                   #-}
{-# OPTIONS -Wno-missing-signatures #-}
{-# OPTIONS -Wno-unused-top-binds   #-}

module DFINITY.RadixTree.Lenses
  ( getPrefix
  , getLeft
  , getRight
  , getChild
  , getChildren
  , getLeaf
  , getPath
  , getValue
  , getBuffer
  , getCache
  , getCheckpoint
  , getNonce
  , getRoot
  , setPrefix
  , setLeft
  , setRight
  , setChild
  , setChildren
  , setLeaf
  , setBuffer
  , setCache
  , setCheckpoint
  , setNonce
  , setRoot
  )
where

import Data.Bool
import Data.ByteString
import Data.List.NonEmpty
import Data.Word

import DFINITY.RadixTree.Types

getPrefix :: RadixNode -> Maybe RadixPrefix
getPrefix RadixNode {_radixPrefix} = _radixPrefix

getLeft :: RadixNode -> Maybe RadixRoot
getLeft RadixNode {_radixLeft} = _radixLeft

getRight :: RadixNode -> Maybe RadixRoot
getRight RadixNode {_radixRight} = _radixRight

getChild :: Bool -> RadixNode -> Maybe RadixRoot
getChild = bool getLeft getRight

getChildren :: RadixNode -> (Maybe RadixRoot, Maybe RadixRoot)
getChildren node = (getLeft node, getRight node)

getLeaf :: RadixNode -> Maybe ByteString
getLeaf RadixNode {_radixLeaf} = _radixLeaf

getPath :: RadixProof -> NonEmpty RadixNode
getPath RadixProof {_radixPath} = _radixPath

getValue :: RadixProof -> ByteString
getValue RadixProof {_radixValue} = _radixValue

getBuffer :: RadixTree database -> RadixBuffer
getBuffer RadixTree {_radixBuffer} = _radixBuffer

getCache :: RadixTree database -> RadixCache
getCache RadixTree {_radixCache} = _radixCache

getCheckpoint :: RadixTree database -> RadixRoot
getCheckpoint RadixTree {_radixCheckpoint} = _radixCheckpoint

getNonce :: RadixTree database -> Word32
getNonce RadixTree {_radixNonce} = _radixNonce

getRoot :: RadixTree database -> RadixRoot
getRoot RadixTree {_radixRoot} = _radixRoot

setPrefix :: Maybe RadixPrefix -> RadixNode -> RadixNode
setPrefix prefix node = node {_radixPrefix = prefix}

setLeft :: Maybe RadixRoot -> RadixNode -> RadixNode
setLeft left node = node {_radixLeft = left}

setRight :: Maybe RadixRoot -> RadixNode -> RadixNode
setRight right node = node {_radixRight = right}

setChild :: Bool -> Maybe RadixRoot -> RadixNode -> RadixNode
setChild = bool setLeft setRight

setChildren :: (Maybe RadixRoot, Maybe RadixRoot) -> RadixNode -> RadixNode
setChildren (left, right) = setLeft left . setRight right

setLeaf :: Maybe ByteString -> RadixNode -> RadixNode
setLeaf leaf node = node {_radixLeaf = leaf}

setPath :: NonEmpty RadixNode -> RadixProof -> RadixProof
setPath path proof = proof {_radixPath = path}

setValue :: ByteString -> RadixProof -> RadixProof
setValue value proof = proof {_radixValue = value}

setBuffer :: RadixBuffer -> RadixTree database -> RadixTree database
setBuffer buffer tree = tree {_radixBuffer = buffer}

setCache :: RadixCache -> RadixTree database -> RadixTree database
setCache cache tree = tree {_radixCache = cache}

setCheckpoint :: RadixRoot -> RadixTree database -> RadixTree database
setCheckpoint checkpoint tree = tree {_radixCheckpoint = checkpoint}

setNonce :: Word32 -> RadixTree database -> RadixTree database
setNonce nonce tree = tree {_radixNonce = nonce}

setRoot :: RadixRoot -> RadixTree database -> RadixTree database
setRoot root tree = tree {_radixRoot = root}

