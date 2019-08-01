{-# LANGUAGE ExplicitForAll  #-}
{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections   #-}
{-# LANGUAGE ViewPatterns    #-}

{-# OPTIONS -Wall                 #-}
{-# OPTIONS -Wno-unused-top-binds #-}

-- |
-- Module     : DFINITY.RadixTree
-- Copyright  : 2018-2019 DFINITY Stiftung
-- License    : BSD-3-Clause
-- Maintainer : Enzo Haussecker <enzo@dfinity.org>
-- Stability  : Stable
--
-- A generic data integrity layer.
module DFINITY.RadixTree
  (

  -- ** Class
    RadixDatabase(..)

  -- ** Types
  , RadixError(..)
  , RadixProof
  , RadixRoot
  , RadixTree

  -- ** Getters
  , getCheckpoint
  , getValue

  -- ** Create
  , createRadixTree

  -- ** Insert
  , insertRadixTree

  -- ** Delete
  , deleteRadixTree

  -- ** Merkleize
  , merkleizeRadixTree

  -- ** Query
  , lookupRadixTree

  -- ** Prove
  , createRadixProof

  -- ** Verify
  , verifyRadixProof

  -- ** Test
  , isEmptyRadixTree
  , isValidRadixRoot

  -- ** Subtrees
  , subtreeRadixTree

  -- ** Debug

   -- *** Contents
  , contentsRadixTree
  , contentsMerkleizedRadixTree
  , contentsNonMerkleizedRadixTree

   -- *** Draw
  , drawRadixTree
  , drawMerkleizedRadixTree
  , drawNonMerkleizedRadixTree

   -- *** Print
  , printRadixTree
  , printMerkleizedRadixTree
  , printNonMerkleizedRadixTree
  )
where

import Control.Applicative
import Control.Exception
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Resource
import Data.Bool
import Data.ByteString.Base16       as Base16
import Data.ByteString.Char8        as Strict
import Data.ByteString.Short        as Short
import Data.Default.Class
import Data.DList                   as DList
import Data.Functor
import Data.Functor.Reverse
import Data.List                    as List
import Data.List.NonEmpty           as NonEmpty
import Data.LruCache                as LRU
import Data.Map.Strict              as Map
import Data.Maybe
import Data.Semigroup.Applicative
import Data.Tuple
import Data.Word
import Database.LevelDB             as LevelDB
import System.IO                    as IO

import DFINITY.RadixTree.Bits
import DFINITY.RadixTree.Lenses
import DFINITY.RadixTree.Memory
import DFINITY.RadixTree.Types
import DFINITY.RadixTree.Utilities

--------------------------------------------------------------------------------

-- |
-- Create a radix tree.
createRadixTree
  :: RadixDatabase m database
  => Int
  -- ^ Cache size.
  -> Maybe RadixRoot
  -- ^ State root.
  -> database
  -- ^ Database.
  -> m (RadixTree database)
createRadixTree cacheSize checkpoint database
  | cacheSize <= 0 = throw $ InvalidArgument "invalid LRU cache size"
  | otherwise = do
    (root, cache') <- case checkpoint of
      Nothing -> do
        storeCold def cache database
      Just root -> do
        result <- loadCold root cache database
        case snd <$> result of
          Nothing -> do
            throw $ StateRootDoesNotExist root
          Just cache' -> do
            return (root, cache')
    return $ RadixTree mempty cache' cacheSize root database 0 root
  where cache = LRU.empty cacheSize

{-# SPECIALISE createRadixTree
  :: Int
  -> Maybe RadixRoot
  -> LevelDB.DB
  -> ResourceT IO (RadixTree LevelDB.DB) #-}

--------------------------------------------------------------------------------

-- |
-- Search for a value in a radix tree.
searchRadixTree
  :: RadixDatabase m database
  => Bool
  -- ^ Overwrite state root?
  -> (RadixTree database -> m (Maybe (RadixNode, RadixCache)))
  -- ^ Loading strategy.
  -> ByteString
  -- ^ Key.
  -> RadixTree database
  -- ^ Radix tree.
  -> m (Either RadixError RadixSearchResult)
searchRadixTree flag strategy = \key tree@RadixTree {..} -> do
  let key'  = toBits key
  let tree' = tree `bool` setRoot _radixCheckpoint tree $ flag
  loop Nothing [] [] [] key' tree'
 where

  -- Loop.
  loop implicit roots nodes prefixes key tree@RadixTree {..} = do

    -- Load the root node.
    result <- strategy tree
    case result of
      Nothing -> do
        return $ Left $ StateRootDoesNotExist _radixRoot
      Just (node@RadixNode {..}, cache') -> do

        -- Calculate the matching prefix and overflow bits.
        let bits = maybe id (:) implicit $ maybe [] toBits _radixPrefix
        let prefix    = matchBits bits key
        let n         = List.length prefix
        let overflow  = List.drop n bits
        let residue   = not $ List.null overflow

        -- Update the accumulators.
        let roots'    = _radixRoot : roots
        let nodes'    = node : nodes
        let prefixes' = prefix : prefixes
        let key'      = List.drop n key

        -- Check the termination criteria.
        let bit       = List.head key'
        let child     = bool _radixLeft _radixRight bit
        if List.null key' || residue || isNothing child

          -- Terminate.
          then return $ Right
            ( NonEmpty.fromList roots'
            , NonEmpty.fromList nodes'
            , NonEmpty.fromList prefixes'
            , overflow
            , key'
            , cache'
            )

          -- Recurse.
          else do
            let root'     = fromJust child
            let tree'     = setCache cache' $ setRoot root' tree
            let implicit' = Just bit
            loop implicit' roots' nodes' prefixes' key' tree'

{-# SPECIALISE searchRadixTree
  :: Bool
  -> (RadixTree LevelDB.DB -> ResourceT IO (Maybe (RadixNode, RadixCache)))
  -> ByteString
  -> RadixTree LevelDB.DB
  -> ResourceT IO (Either RadixError RadixSearchResult) #-}

--------------------------------------------------------------------------------

-- |
-- Search for a value in a Merkleized radix tree.
searchMerkleizedRadixTree
  :: RadixDatabase m database
  => ByteString
  -- ^ Key.
  -> RadixTree database
  -- ^ Radix tree.
  -> m (Either RadixError RadixSearchResult)
searchMerkleizedRadixTree = do
  searchRadixTree True $ \RadixTree {..} -> do
    loadCold _radixRoot _radixCache _radixDatabase

{-# SPECIALISE searchMerkleizedRadixTree
  :: ByteString
  -> RadixTree LevelDB.DB
  -> ResourceT IO (Either RadixError RadixSearchResult) #-}

--------------------------------------------------------------------------------

-- |
-- Search for a value in a non-Merkleized radix tree.
searchNonMerkleizedRadixTree
  :: RadixDatabase m database
  => ByteString
  -- ^ Key.
  -> RadixTree database
  -- ^ Radix tree.
  -> m (Either RadixError RadixSearchResult)
searchNonMerkleizedRadixTree = do
  searchRadixTree False $ \RadixTree {..} -> do
    loadHot _radixRoot _radixBuffer _radixCache _radixDatabase

{-# SPECIALISE searchNonMerkleizedRadixTree
  :: ByteString
  -> RadixTree LevelDB.DB
  -> ResourceT IO (Either RadixError RadixSearchResult) #-}

--------------------------------------------------------------------------------

-- |
-- Insert a value into a radix tree.
insertRadixTree
  :: RadixDatabase m database
  => ByteString
  -- ^ Key.
  -> ByteString
  -- ^ Value.
  -> RadixTree database
  -- ^ Radix tree.
  -> m (RadixTree database)
insertRadixTree key value tree = if isEmptyRadixTree tree
  then return $ initializeRadixTree key value tree
  else searchNonMerkleizedRadixTree key tree >>= \case
    Left err -> throw err
    Right result@(_, _, _, [], [], _) ->
      return $ insertRadixTreeAt result value tree
    Right result@(_, _, _, [], _, _) ->
      return $ insertRadixTreeAfter result value tree
    Right result@(_, _, _, _, [], _) ->
      return $ insertRadixTreeBefore result value tree
    Right result -> return $ insertRadixTreeBetween result value tree

{-# SPECIALISE insertRadixTree
  :: ByteString
  -> ByteString
  -> RadixTree LevelDB.DB
  -> ResourceT IO (RadixTree LevelDB.DB) #-}

--------------------------------------------------------------------------------

-- TODO (enzo): Documentation.
initializeRadixTree
  :: ByteString
  -- ^ Key.
  -> ByteString
  -- ^ Value.
  -> RadixTree database
  -- ^ Radix tree.
  -> RadixTree database
initializeRadixTree key value tree@RadixTree {..} =
  setBuffer buffer $ setNonce nonce $ setRoot root tree
 where
  prefix = createPrefix $ toBits key
  root   = createRootFromNonce _radixNonce
  node   = setPrefix prefix $ Just value `setLeaf` def
  buffer = Map.insert root node _radixBuffer
  nonce  = _radixNonce + 1

{-# INLINABLE initializeRadixTree #-}

--------------------------------------------------------------------------------

-- TODO (enzo): Documentation.
insertRadixTreeAt
  :: RadixSearchResult
  -- ^ Search result.
  -> ByteString
  -- ^ Value.
  -> RadixTree database
  -- ^ Radix tree.
  -> RadixTree database
insertRadixTreeAt (root :| roots, node :| nodes, prefix :| prefixes, _, _, cache) value tree@RadixTree {..}
  = setBuffer buffer $ setCache cache $ setNonce nonce $ setRoot state tree
 where
  root'   = createRootFromNonce _radixNonce
  node'   = Just value `setLeaf` node
  parents = createParents roots nodes prefix prefixes
  counter = _radixNonce + 1
  (,,) buffer nonce state =
    merkleSpoof parents root' counter $ Map.insert root' node' $ Map.delete
      root
      _radixBuffer

{-# INLINABLE insertRadixTreeAt #-}

--------------------------------------------------------------------------------

-- TODO (enzo): Documentation.
insertRadixTreeAfter
  :: RadixSearchResult
  -- ^ Search result.
  -> ByteString
  -- ^ Value.
  -> RadixTree database
  -- ^ Radix tree.
  -> RadixTree database
insertRadixTreeAfter (root :| roots, node :| nodes, prefix :| prefixes, _, keyOverflow, cache) value tree@RadixTree {..}
  = setBuffer buffer $ setCache cache $ setNonce nonce $ setRoot state tree
 where
  prefix' = createPrefix $ List.drop 1 keyOverflow
  root'   = createRootFromNonce _radixNonce
  node'   = setPrefix prefix' $ Just value `setLeaf` def
  root''  = createRootFromNonce $ _radixNonce + 1
  node''  = test `setChild` Just root' $ node
  test    = List.head keyOverflow
  parents = createParents roots nodes prefix prefixes
  counter = _radixNonce + 2
  (,,) buffer nonce state =
    merkleSpoof parents root'' counter
      $ Map.insert root'' node''
      $ Map.insert root' node'
      $ Map.delete root _radixBuffer

{-# INLINABLE insertRadixTreeAfter #-}

--------------------------------------------------------------------------------

-- TODO (enzo): Documentation.
insertRadixTreeBefore
  :: RadixSearchResult
  -- ^ Search result.
  -> ByteString
  -- ^ Value.
  -> RadixTree database
  -- ^ Radix tree.
  -> RadixTree database
insertRadixTreeBefore (root :| roots, node :| nodes, prefix :| prefixes, prefixOverflow, _, cache) value tree@RadixTree {..}
  = setBuffer buffer $ setCache cache $ setNonce nonce $ setRoot state tree
 where
  prefix' = createPrefix $ List.drop 1 prefixOverflow
  root'   = createRootFromNonce _radixNonce
  node'   = setPrefix prefix' node
  prefix'' =
    createPrefix $ List.drop 1 prefix `bool` prefix $ List.null parents
  root'' = createRootFromNonce $ _radixNonce + 1
  node'' =
    setPrefix prefix'' $ test `setChild` Just root' $ Just value `setLeaf` def
  test    = List.head prefixOverflow
  parents = createParents roots nodes prefix prefixes
  counter = _radixNonce + 2
  (,,) buffer nonce state =
    merkleSpoof parents root'' counter
      $ Map.insert root'' node''
      $ Map.insert root' node'
      $ Map.delete root _radixBuffer

{-# INLINABLE insertRadixTreeBefore #-}

--------------------------------------------------------------------------------

-- TODO (enzo): Documentation.
insertRadixTreeBetween
  :: RadixSearchResult
  -- ^ Search result.
  -> ByteString
  -- ^ Value.
  -> RadixTree database
  -- ^ Radix tree.
  -> RadixTree database
insertRadixTreeBetween (root :| roots, node :| nodes, prefix :| prefixes, prefixOverflow, keyOverflow, cache) value tree@RadixTree {..}
  = setBuffer buffer $ setCache cache $ setNonce nonce $ setRoot state tree
 where
  prefix'  = createPrefix $ List.drop 1 keyOverflow
  root'    = createRootFromNonce _radixNonce
  node'    = setPrefix prefix' $ Just value `setLeaf` def
  prefix'' = createPrefix $ List.drop 1 prefixOverflow
  root''   = createRootFromNonce $ _radixNonce + 1
  node''   = setPrefix prefix'' node
  prefix''' =
    createPrefix $ List.drop 1 prefix `bool` prefix $ List.null parents
  root'''  = createRootFromNonce $ _radixNonce + 2
  node'''  = setPrefix prefix''' $ setChildren children def
  test     = List.head keyOverflow
  children = bool id swap test (Just root', Just root'')
  parents  = createParents roots nodes prefix prefixes
  counter  = _radixNonce + 3
  (,,) buffer nonce state =
    merkleSpoof parents root''' counter
      $ Map.insert root''' node'''
      $ Map.insert root'' node''
      $ Map.insert root' node'
      $ Map.delete root _radixBuffer

{-# INLINABLE insertRadixTreeBetween #-}

--------------------------------------------------------------------------------

-- |
-- Delete a value from a radix tree.
deleteRadixTree
  :: RadixDatabase m database
  => ByteString
  -- ^ Key.
  -> RadixTree database
  -- ^ Radix tree.
  -> m (RadixTree database)
deleteRadixTree key tree@RadixTree {..} = if isEmptyRadixTree tree
  then return tree
  else searchNonMerkleizedRadixTree key tree >>= \case
    Left  err -> throw err
    Right result@(_, nodes, prefix :| _, [], [], cache) -> do
      case nodes of

        -- No children and no parent.
        RadixNode _ Nothing Nothing _ :| [] ->
          return $ deleteRadixTreeNoChildrenNoParent result tree

        -- No children and parent with leaf.
        RadixNode _ Nothing Nothing _ :| parent : _ | isJust $ getLeaf parent ->
          return $ deleteRadixTreeNoChildrenParentWithLeaf result tree

        -- No children and parent without leaf.
        RadixNode _ Nothing Nothing _ :| parent : _ -> do
          let test = not $ List.head prefix
          let root = fromJust $ getChild test parent
          loadHot root _radixBuffer cache _radixDatabase >>= \case
            Nothing -> do
              throw $ StateRootDoesNotExist root
            Just (node, cache') ->
              return $ deleteRadixTreeNoChildrenParentWithoutLeaf result
                                                                  node
                                                                  cache'
                                                                  test
                                                                  tree

        -- One left child.
        RadixNode _ child Nothing _ :| _ | isJust child -> do
          let test = False
          let root = fromJust child
          loadHot root _radixBuffer cache _radixDatabase >>= \case
            Nothing -> do
              throw $ StateRootDoesNotExist root
            Just (node, cache') ->
              return $ deleteRadixTreeOneChild result node cache' test tree

        -- One right child.
        RadixNode _ Nothing child _ :| _ | isJust child -> do
          let test = True
          let root = fromJust child
          loadHot root _radixBuffer cache _radixDatabase >>= \case
            Nothing -> do
              throw $ StateRootDoesNotExist root
            Just (node, cache') ->
              return $ deleteRadixTreeOneChild result node cache' test tree

        -- Two children.
        _ -> return $ deleteRadixTreeTwoChildren result tree

    Right _ -> return tree

{-# SPECIALISE deleteRadixTree
  :: ByteString
  -> RadixTree LevelDB.DB
  -> ResourceT IO (RadixTree LevelDB.DB) #-}

--------------------------------------------------------------------------------

-- TODO (enzo): Documentation.
deleteRadixTreeNoChildrenNoParent
  :: RadixSearchResult
  -- ^ Search result.
  -> RadixTree database
  -- ^ Radix tree.
  -> RadixTree database
deleteRadixTreeNoChildrenNoParent (root :| _, _, _, _, _, cache) tree@RadixTree {..}
  = setBuffer buffer $ setCache cache $ setRoot state tree
 where
  buffer = Map.insert defaultRoot def $ Map.delete root _radixBuffer
  state  = defaultRoot

{-# INLINABLE deleteRadixTreeNoChildrenNoParent #-}

--------------------------------------------------------------------------------

-- TODO (enzo): Documentation.
deleteRadixTreeNoChildrenParentWithLeaf
  :: RadixSearchResult
  -- ^ Search result.
  -> RadixTree database
  -- ^ Radix tree.
  -> RadixTree database
deleteRadixTreeNoChildrenParentWithLeaf (_ :| root : roots, _ :| node : nodes, overflow :| prefix : prefixes, _, _, cache) tree@RadixTree {..}
  = setBuffer buffer $ setCache cache $ setNonce nonce $ setRoot state tree
 where
  root'   = createRootFromNonce _radixNonce
  node'   = setChild test Nothing node
  test    = List.head overflow
  parents = createParents roots nodes prefix prefixes
  counter = _radixNonce + 1
  (,,) buffer nonce state =
    merkleSpoof parents root' counter $ Map.insert root' node' $ Map.delete
      root
      _radixBuffer
deleteRadixTreeNoChildrenParentWithLeaf _ _ =
  throw $ InvalidArgument "unknown parent"

{-# INLINABLE deleteRadixTreeNoChildrenParentWithLeaf #-}

--------------------------------------------------------------------------------

-- TODO (enzo): Documentation.
deleteRadixTreeNoChildrenParentWithoutLeaf
  :: RadixSearchResult
  -- ^ Search result.
  -> RadixNode
  -- ^ Radix node.
  -> RadixCache
  -- ^ Radix cache.
  -> Bool
  -- ^ Lineage.
  -> RadixTree database
  -- ^ Radix tree.
  -> RadixTree database
deleteRadixTreeNoChildrenParentWithoutLeaf (_ :| root : roots, _ :| _ : nodes, _ :| prefix : prefixes, _, _, _) node@RadixNode {..} cache test tree@RadixTree {..}
  = setBuffer buffer $ setCache cache $ setNonce nonce $ setRoot state tree
 where
  prefix' = createPrefix $ List.drop 1 bits `bool` bits $ List.null parents
  root'   = createRootFromNonce _radixNonce
  node'   = setPrefix prefix' node
  bits    = prefix ++ test : maybe [] toBits _radixPrefix
  parents = createParents roots nodes prefix prefixes
  counter = _radixNonce + 1
  (,,) buffer nonce state =
    merkleSpoof parents root' counter $ Map.insert root' node' $ Map.delete
      root
      _radixBuffer
deleteRadixTreeNoChildrenParentWithoutLeaf _ _ _ _ _ =
  throw $ InvalidArgument "unknown parent"

{-# INLINABLE deleteRadixTreeNoChildrenParentWithoutLeaf #-}

--------------------------------------------------------------------------------

-- TODO (enzo): Documentation.
deleteRadixTreeOneChild
  :: RadixSearchResult
  -- ^ Search result.
  -> RadixNode
  -- ^ Radix node.
  -> RadixCache
  -- ^ Radix cache.
  -> Bool
  -- ^ Lineage.
  -> RadixTree database
  -- ^ Radix tree.
  -> RadixTree database
deleteRadixTreeOneChild (root :| roots, _ :| nodes, prefix :| prefixes, _, _, _) node@RadixNode {..} cache test tree@RadixTree {..}
  = setBuffer buffer $ setCache cache $ setNonce nonce $ setRoot state tree
 where
  prefix' = createPrefix $ List.drop 1 bits `bool` bits $ List.null parents
  root'   = createRootFromNonce _radixNonce
  node'   = setPrefix prefix' node
  bits    = prefix ++ test : maybe [] toBits _radixPrefix
  parents = createParents roots nodes prefix prefixes
  counter = _radixNonce + 1
  (,,) buffer nonce state =
    merkleSpoof parents root' counter $ Map.insert root' node' $ Map.delete
      root
      _radixBuffer

{-# INLINABLE deleteRadixTreeOneChild #-}

--------------------------------------------------------------------------------

-- TODO (enzo): Documentation.
deleteRadixTreeTwoChildren
  :: RadixSearchResult
  -- ^ Search result.
  -> RadixTree database
  -- ^ Radix tree.
  -> RadixTree database
deleteRadixTreeTwoChildren (root :| roots, node :| nodes, prefix :| prefixes, _, _, cache) tree@RadixTree {..}
  = setBuffer buffer $ setCache cache $ setNonce nonce $ setRoot state tree
 where
  root'   = createRootFromNonce _radixNonce
  node'   = setLeaf Nothing node
  parents = createParents roots nodes prefix prefixes
  counter = _radixNonce + 1
  (,,) buffer nonce state =
    merkleSpoof parents root' counter $ Map.insert root' node' $ Map.delete
      root
      _radixBuffer

{-# INLINABLE deleteRadixTreeTwoChildren #-}

--------------------------------------------------------------------------------

-- |
-- Lookup a value in a radix tree.
lookupRadixTree
  :: RadixDatabase m database
  => ByteString
  -- ^ Key.
  -> RadixTree database
  -- ^ Radix tree.
  -> m (Maybe (ByteString, RadixTree database))
lookupRadixTree key tree = do
  found <- searchNonMerkleizedRadixTree key tree
  case found of
    Left err -> throw err
    Right (_, RadixNode {..} :| _, _, prefixOverflow, keyOverflow, cache') ->
      if not $ List.null prefixOverflow && List.null keyOverflow
        then return Nothing
        else return $ do
          value <- _radixLeaf
          let tree' = setCache cache' tree
          return (value, tree')

{-# SPECIALISE lookupRadixTree
  :: ByteString
  -> RadixTree LevelDB.DB
  -> ResourceT IO (Maybe (ByteString, RadixTree LevelDB.DB)) #-}

--------------------------------------------------------------------------------

-- |
-- Prove that a value exists in a radix tree.
createRadixProof
  :: RadixDatabase m database
  => ByteString
  -- ^ Key.
  -> RadixTree database
  -- ^ Radix tree.
  -> m (Maybe (RadixProof, RadixTree database))
createRadixProof key tree = do
  found <- searchMerkleizedRadixTree key tree
  case found of
    Left err -> throw err
    Right (_, path, _, prefixOverflow, keyOverflow, cache') ->
      if not $ List.null prefixOverflow && List.null keyOverflow
        then return Nothing
        else return $ do
          value <- _radixLeaf $ NonEmpty.head path
          let tree' = setCache cache' tree
          let leaf' = setLeaf Nothing $ NonEmpty.head path
          let path' = leaf' :| NonEmpty.tail path
          let proof = RadixProof path' value
          return (proof, tree')

{-# SPECIALISE createRadixProof
  :: ByteString
  -> RadixTree LevelDB.DB
  -> ResourceT IO (Maybe (RadixProof, RadixTree LevelDB.DB)) #-}

--------------------------------------------------------------------------------

-- |
-- Verify that a value exists in a radix tree.
verifyRadixProof
  :: ByteString
  -- ^ Key.
  -> RadixRoot
  -- ^ State root.
  -> RadixProof
  -- ^ Radix proof.
  -> Bool
verifyRadixProof key rootHash (includeValue -> RadixProof{..}) =
  validateKey && validateHashes
 where
    -- Validate the proof is for the value at the given key &
    -- validate that the hashes are correct (both state root matches
    -- and that child hashes are found in their parents)
  (validateKey, validateHashes) = fromMaybe (False, False) $ do
    keyBits <- recoverKey
    let root = NonEmpty.last _radixPath
    return (keyBits == toBits key, createRoot root == rootHash)

  recoverKey = do
    let childParents =
          List.zip (NonEmpty.toList _radixPath) (NonEmpty.tail _radixPath)
    leaflessBits <- getAp $ foldMap (Ap . recoverBits) (Reverse childParents)
    let keyBits = leaflessBits <> DList.fromList (getPrefixBits proofLeaf)
    return $ DList.toList keyBits

  getPrefixBits = maybe [] toBits . _radixPrefix

  proofLeaf     = NonEmpty.head _radixPath

  -- Recovering the prefix necessitates us validating the child/parent
  -- hashes. This is because we must find use the child's hash to
  -- decide if the implicit bit is 0 or 1
  recoverBits (child, parent) = do
    let prefixBits = maybe [] toBits (_radixPrefix parent)
        childHash  = createRoot child
        implicit0  = mfilter (== childHash) (_radixLeft parent) $> False
        implicit1  = mfilter (== childHash) (_radixRight parent) $> True

    -- This returning Nothing means we could not validate child/parent
    implicit <- implicit0 <|> implicit1

    return $ DList.fromList prefixBits <> DList.singleton implicit

-- Add the missing leaf value to _radixPath
includeValue :: RadixProof -> RadixProof
includeValue RadixProof {..} = RadixProof
  { _radixPath  = setLeaf (Just _radixValue) (NonEmpty.head _radixPath)
    :| NonEmpty.tail _radixPath
  , _radixValue = _radixValue
  }

--------------------------------------------------------------------------------

-- |
-- Check if a radix tree is empty.
isEmptyRadixTree
  :: RadixTree database
  -- ^ Radix tree.
  -> Bool
isEmptyRadixTree tree = _radixRoot tree == defaultRoot

{-# INLINABLE isEmptyRadixTree #-}

--------------------------------------------------------------------------------

-- |
-- Check if a state root is valid.
isValidRadixRoot
  :: RadixDatabase m database
  => RadixRoot
  -- ^ State root.
  -> RadixTree database
  -- ^ Radix tree.
  -> m Bool
isValidRadixRoot root RadixTree {..} = isJust <$> load _radixDatabase key
  where key = fromShort root

{-# SPECIALISE isValidRadixRoot
  :: RadixRoot
  -> RadixTree LevelDB.DB
  -> ResourceT IO Bool #-}

--------------------------------------------------------------------------------

-- TODO (enzo): Documentation.
merkleSpoof
  :: [(RadixRoot, RadixNode, Bool)]
  -> RadixRoot
  -> Word32
  -> RadixBuffer
  -> (RadixBuffer, Word32, RadixRoot)
merkleSpoof parents fake counter buffer = do
  case parents of
    []                             -> (,,) buffer counter fake
    (,,) root node test : parents' -> do
      let node'    = test `setChild` Just fake $ node
      let root'    = createRootFromNonce counter
      let counter' = counter + 1
      merkleSpoof parents' root' counter' $ Map.insert root' node' $ Map.delete
        root
        buffer

--------------------------------------------------------------------------------

-- |
-- Merkleize a radix tree. This will flush the buffer to the database.
merkleizeRadixTree
  :: RadixDatabase m database
  => RadixTree database
  -- ^ Radix tree.
  -> m (RadixRoot, RadixTree database)
merkleizeRadixTree RadixTree {..} = do
  (,) root cache <- loop _radixRoot _radixCache
  let tree = RadixTree mempty cache _radixCacheSize root _radixDatabase 0 root
  return (root, tree)
 where

  -- Loop.
  loop root cache = do

    -- WARNING: Here we distinguish between Merkleized and non-Merkleized roots
    -- by size. Merkleized roots are 20-byte hashes whereas non-Merkleized roots
    -- are 4-byte nonces. This is a shaky assumption that is subject to change.
    if Short.length root == 20
      then return (root, cache)
      else do

        -- Load the root node.
        result <- loadHot root _radixBuffer cache _radixDatabase
        case result of
          Nothing -> do
            throw $ StateRootDoesNotExist root
          Just (node@RadixNode {..}, cache') -> do
            case (_radixLeft, _radixRight) of

              -- No children.
              (Nothing, Nothing) -> do
                storeCold node cache' _radixDatabase

              -- One left child.
              (Just child, Nothing) -> do
                (root', cache'') <- loop child cache'
                let node' = False `setChild` Just root' $ node
                storeCold node' cache'' _radixDatabase

              -- One right child.
              (Nothing, Just child) -> do
                (root', cache'') <- loop child cache'
                let node' = True `setChild` Just root' $ node
                storeCold node' cache'' _radixDatabase

              -- Two children.
              (Just left, Just right) -> do
                (root' , cache'' ) <- loop left cache'
                (root'', cache''') <- loop right cache''
                let node' = setChildren (Just root', Just root'') node
                storeCold node' cache''' _radixDatabase

{-# SPECIALISE merkleizeRadixTree
  :: RadixTree LevelDB.DB
  -> ResourceT IO (RadixRoot, RadixTree LevelDB.DB) #-}

--------------------------------------------------------------------------------

-- |
-- Create a radix subtree.
subtreeRadixTree
  :: RadixDatabase m database
  => ByteString
  -- ^ Key.
  -> RadixTree database
  -- ^ Radix tree.
  -> m (Maybe (RadixTree database))
subtreeRadixTree key tree@RadixTree {..} = do
  found <- searchNonMerkleizedRadixTree key tree
  case found of
    Left err -> throw err
    Right (_, node :| _, _, prefixOverflow, keyOverflow, cache) ->
      if not $ List.null keyOverflow
        then return Nothing
        else do
          let prefix' = createPrefix prefixOverflow
          let root'   = createRootFromNonce _radixNonce
          let node'   = setPrefix prefix' node
          let buffer  = Map.insert root' node' _radixBuffer
          let nonce   = _radixNonce + 1
          return
            $ Just
            $ setBuffer buffer
            $ setCache cache
            $ setNonce nonce
            $ setRoot root' tree

{-# SPECIALISE subtreeRadixTree
  :: ByteString
  -> RadixTree LevelDB.DB
  -> ResourceT IO (Maybe (RadixTree LevelDB.DB)) #-}

--------------------------------------------------------------------------------

-- |
-- Get the contents of a radix tree.
contentsRadixTree'
  :: RadixDatabase m database
  => Bool
  -- ^ Overwrite state root?
  -> (RadixTree database -> m (Maybe (RadixNode, RadixCache)))
  -- ^ Loading strategy.
  -> RadixTree database
  -- ^ Radix tree.
  -> m [(ByteString, ByteString)]
contentsRadixTree' flag strategy = \tree@RadixTree {..} -> do
  let tree' = tree `bool` setRoot _radixCheckpoint tree $ flag
  loop tree' [] []
 where

  -- Loop.
  loop tree@RadixTree {..} prefix accum = do
    result <- strategy tree
    case fst <$> result of
      Nothing -> do
        throw $ StateRootDoesNotExist _radixRoot
      Just RadixNode {..} -> do
        let prefix'  = prefix ++ maybe [] toBits _radixPrefix
        let key      = fromBits prefix'
        let accum' = maybe accum (\value -> (key, value) : accum) _radixLeaf
        let children = [(, False) <$> _radixLeft, (, True) <$> _radixRight]
        flip foldM accum' `flip` children $ \accum'' -> \case
          Nothing -> do
            return accum''
          Just (root, test) -> do
            let tree'    = setRoot root tree
            let prefix'' = prefix' ++ [test]
            loop tree' prefix'' accum''

{-# SPECIALISE contentsRadixTree'
  :: Bool
  -> (RadixTree LevelDB.DB -> ResourceT IO (Maybe (RadixNode, RadixCache)))
  -> RadixTree LevelDB.DB
  -> ResourceT IO [(ByteString, ByteString)] #-}

--------------------------------------------------------------------------------

-- |
-- A convenient alias for `contentsNonMerkleizedRadixTree`.
contentsRadixTree
  :: RadixDatabase m database
  => RadixTree database
  -- ^ Radix tree.
  -> m [(ByteString, ByteString)]
contentsRadixTree = contentsNonMerkleizedRadixTree

{-# SPECIALISE contentsRadixTree
  :: RadixTree LevelDB.DB
  -> ResourceT IO [(ByteString, ByteString)] #-}

--------------------------------------------------------------------------------

-- |
-- Get the contents of a Merkleized radix tree.
contentsMerkleizedRadixTree
  :: RadixDatabase m database
  => RadixTree database
  -- ^ Radix tree.
  -> m [(ByteString, ByteString)]
contentsMerkleizedRadixTree = do
  contentsRadixTree' True $ \RadixTree {..} -> do
    loadCold _radixRoot _radixCache _radixDatabase

{-# SPECIALISE contentsMerkleizedRadixTree
  :: RadixTree LevelDB.DB
  -> ResourceT IO [(ByteString, ByteString)] #-}

--------------------------------------------------------------------------------

-- |
-- Get the contents of a non-Merkleized radix tree.
contentsNonMerkleizedRadixTree
  :: RadixDatabase m database
  => RadixTree database
  -- ^ Radix tree.
  -> m [(ByteString, ByteString)]
contentsNonMerkleizedRadixTree = do
  contentsRadixTree' False $ \RadixTree {..} -> do
    loadHot _radixRoot _radixBuffer _radixCache _radixDatabase

{-# SPECIALISE contentsNonMerkleizedRadixTree
  :: RadixTree LevelDB.DB
  -> ResourceT IO [(ByteString, ByteString)] #-}

--------------------------------------------------------------------------------

-- |
-- Draw a radix tree.
drawRadixTree'
  :: RadixDatabase m database
  => Bool
  -- ^ Overwrite state root?
  -> (RadixTree database -> m (Maybe (RadixNode, RadixCache)))
  -- ^ Loading strategy.
  -> RadixTree database
  -- ^ Radix tree.
  -> m String
drawRadixTree' flag strategy = \tree@RadixTree {..} -> do
  let tree' = tree `bool` setRoot _radixCheckpoint tree $ flag
  List.unlines <$> loop1 tree'
 where

  -- Loop 1.
  loop1 tree@RadixTree {..} = do
    result <- strategy tree
    case fst <$> result of
      Nothing -> do
        throw $ StateRootDoesNotExist _radixRoot
      Just node@RadixNode {..} -> do
        let children = catMaybes [_radixRight, _radixLeft]
        more <- loop2 $ flip setRoot tree <$> children
        let item = [pretty _radixRoot ++ show node]
        return $ item ++ more

  -- Loop 2.
  loop2 = \case
    []  -> return []
    [x] -> do
      item <- loop1 x
      return $ "│" : shift "└─╴" "   " item
    x : xs -> do
      item <- loop1 x
      more <- loop2 xs
      return $ "│" : shift "├─╴" "│  " item ++ more

  -- Utilities.
  pretty = color . List.take 8 . Strict.unpack . Base16.encode . fromShort
  color  = (++) "\ESC[96m" . flip (++) "\ESC[0m"
  shift x = List.zipWith (++) . (:) x . List.repeat

{-# SPECIALISE drawRadixTree'
  :: Bool
  -> (RadixTree LevelDB.DB -> ResourceT IO (Maybe (RadixNode, RadixCache)))
  -> RadixTree LevelDB.DB
  -> ResourceT IO String #-}

--------------------------------------------------------------------------------

-- |
-- A convenient alias for `drawNonMerkleizedRadixTree`.
drawRadixTree
  :: RadixDatabase m database
  => RadixTree database
  -- ^ Radix tree.
  -> m String
drawRadixTree = drawNonMerkleizedRadixTree

{-# SPECIALISE drawRadixTree
  :: RadixTree LevelDB.DB
  -> ResourceT IO String #-}

--------------------------------------------------------------------------------

-- |
-- Draw a Merkleized radix tree.
drawMerkleizedRadixTree
  :: RadixDatabase m database
  => RadixTree database
  -- ^ Radix tree.
  -> m String
drawMerkleizedRadixTree = do
  drawRadixTree' True $ \RadixTree {..} -> do
    loadCold _radixRoot _radixCache _radixDatabase

{-# SPECIALISE drawMerkleizedRadixTree
  :: RadixTree LevelDB.DB
  -> ResourceT IO String #-}

--------------------------------------------------------------------------------

-- |
-- Draw a non-Merkleized radix tree.
drawNonMerkleizedRadixTree
  :: RadixDatabase m database
  => RadixTree database
  -- ^ Radix tree.
  -> m String
drawNonMerkleizedRadixTree = do
  drawRadixTree' False $ \RadixTree {..} -> do
    loadHot _radixRoot _radixBuffer _radixCache _radixDatabase

{-# SPECIALISE drawNonMerkleizedRadixTree
  :: RadixTree LevelDB.DB
  -> ResourceT IO String #-}

--------------------------------------------------------------------------------

-- |
-- A convenient alias for `printNonMerkleizedRadixTree`.
printRadixTree
  :: MonadIO m
  => RadixDatabase m database
  => RadixTree database
  -- ^ Radix tree.
  -> m ()
printRadixTree = printNonMerkleizedRadixTree

{-# SPECIALISE printRadixTree
  :: RadixTree LevelDB.DB
  -> ResourceT IO () #-}

--------------------------------------------------------------------------------

-- |
-- Print a Merkleized radix tree.
printMerkleizedRadixTree
  :: MonadIO m
  => RadixDatabase m database
  => RadixTree database
  -- ^ Radix tree.
  -> m ()
printMerkleizedRadixTree tree = do
  picture <- drawMerkleizedRadixTree tree
  liftIO $ IO.putStrLn picture

{-# SPECIALISE printMerkleizedRadixTree
  :: RadixTree LevelDB.DB
  -> ResourceT IO () #-}

--------------------------------------------------------------------------------

-- |
-- Print a non-Merkleized radix tree.
printNonMerkleizedRadixTree
  :: MonadIO m
  => RadixDatabase m database
  => RadixTree database
  -- ^ Radix tree.
  -> m ()
printNonMerkleizedRadixTree tree = do
  picture <- drawNonMerkleizedRadixTree tree
  liftIO $ IO.putStrLn picture

{-# SPECIALISE printNonMerkleizedRadixTree
  :: RadixTree LevelDB.DB
  -> ResourceT IO () #-}
