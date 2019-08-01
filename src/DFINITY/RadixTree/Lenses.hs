{-# LANGUAGE TemplateHaskell #-}

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
import Lens.Simple

import DFINITY.RadixTree.Types

makeLenses ''RadixNode
makeLenses ''RadixProof
makeLenses ''RadixTree

getPrefix :: RadixNode -> Maybe RadixPrefix
getPrefix = view radixPrefix

getLeft :: RadixNode -> Maybe RadixRoot
getLeft = view radixLeft

getRight :: RadixNode -> Maybe RadixRoot
getRight = view radixRight

getChild :: Bool -> RadixNode -> Maybe RadixRoot
getChild = bool getLeft getRight

getChildren :: RadixNode -> (Maybe RadixRoot, Maybe RadixRoot)
getChildren node = (getLeft node, getRight node)

getLeaf :: RadixNode -> Maybe ByteString
getLeaf = view radixLeaf

getPath :: RadixProof -> NonEmpty RadixNode
getPath = view radixPath

getValue :: RadixProof -> ByteString
getValue = view radixValue

getBuffer :: RadixTree database -> RadixBuffer
getBuffer = view radixBuffer

getCache :: RadixTree database -> RadixCache
getCache = view radixCache

getCheckpoint :: RadixTree database -> RadixRoot
getCheckpoint = view radixCheckpoint

getNonce :: RadixTree database -> Word32
getNonce = view radixNonce

getRoot :: RadixTree database -> RadixRoot
getRoot = view radixRoot

setPrefix :: Maybe RadixPrefix -> RadixNode -> RadixNode
setPrefix = set radixPrefix

setLeft :: Maybe RadixRoot -> RadixNode -> RadixNode
setLeft = set radixLeft

setRight :: Maybe RadixRoot -> RadixNode -> RadixNode
setRight = set radixRight

setChild :: Bool -> Maybe RadixRoot -> RadixNode -> RadixNode
setChild = bool setLeft setRight

setChildren :: (Maybe RadixRoot, Maybe RadixRoot) -> RadixNode -> RadixNode
setChildren (left, right) = setLeft left . setRight right

setLeaf :: Maybe ByteString -> RadixNode -> RadixNode
setLeaf = set radixLeaf

setPath :: NonEmpty RadixNode -> RadixProof -> RadixProof
setPath = set radixPath

setValue :: ByteString -> RadixProof -> RadixProof
setValue = set radixValue

setBuffer :: RadixBuffer -> RadixTree database -> RadixTree database
setBuffer = set radixBuffer

setCache :: RadixCache -> RadixTree database -> RadixTree database
setCache = set radixCache

setCheckpoint :: RadixRoot -> RadixTree database -> RadixTree database
setCheckpoint = set radixCheckpoint

setNonce :: Word32 -> RadixTree database -> RadixTree database
setNonce = set radixNonce

setRoot :: RadixRoot -> RadixTree database -> RadixTree database
setRoot = set radixRoot
