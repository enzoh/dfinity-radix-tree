{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS -Wall        #-}
{-# OPTIONS -Wno-orphans #-}

module Properties
  ( tests
  )
where

import Control.Monad.Identity
import Control.Monad.State.Strict
import Data.Bifunctor
import Data.ByteString.Char8      as Strict
import Data.ByteString.Short
import Data.List                  as List
import Data.Map                   as Map
import Data.Maybe
import Test.QuickCheck
import Test.Tasty
import Test.Tasty.QuickCheck

import DFINITY.RadixTree
import Types

instance Arbitrary ByteString where
  arbitrary = Strict.pack <$> arbitrary

{-
Here we provide a test to show that the radix tree satisfies the basic
properties of an associative array. Random sequences of insert, delete, and
lookup operations are generated and applied to both the radix tree and a
reference implementation of an associative array. We then compare the results
for equality.
-}

genOps :: Gen [Op]
genOps = do
  len <- getSmall . getPositive <$> arbitrary
  key <- elements <$> vectorOf len arbitrary
  listOf $ frequency
    [ (2, Insert <$> key <*> arbitrary)
    , (2, Delete <$> key)
    , (4, Lookup <$> key <*> pure Nothing)
    , (1, pure $ Merkleize Nothing)
    ]

runReference :: [Op] -> [Maybe ByteString]
runReference ops = snd $ List.mapAccumL go Map.empty ops
 where
  go accum (Insert key value) = (Map.insert key value accum, Nothing)
  go accum (Delete key      ) = (Map.delete key accum, Nothing)
  go accum (Lookup key _    ) = (accum, Map.lookup key accum)
  go accum (Merkleize _     ) = (accum, Nothing)

runRadixTree :: [Op] -> [Maybe ByteString]
runRadixTree ops = flip evalState Map.empty $ unMapDB $ initTree >>= go ops
 where
  go []                           _    = return []
  go (Insert key value : residue) tree = do
    tree' <- insertRadixTree key value tree
    (:) Nothing <$> go residue tree'
  go (Delete key : residue) tree = do
    tree' <- deleteRadixTree key tree
    (:) Nothing <$> go residue tree'
  go (Lookup key _ : residue) tree = do
    result <- lookupRadixTree key tree
    case result of
      Nothing             -> (Nothing :) <$> go residue tree
      Just (value, tree') -> (Just value :) <$> go residue tree'
  go (Merkleize _ : residue) tree = do
    tree' <- snd <$> merkleizeRadixTree tree
    (:) Nothing <$> go residue tree'

prop_lookup :: Property
prop_lookup = forAll genOps $ \ops -> runReference ops === runRadixTree ops

{-
Here we provide a test to show that the contents of a radix tree is uniquely
defined by its root hash, regardless of the order of operations applied. Random
sequences sequences of insert, delete, lookup, and merkelize operations are
generated and applied in random order to the same tree. The final root hashs are
then calculated and tested for equality.
-}

genMap :: Gen (Map ByteString ByteString)
genMap = fromList . List.map convert . toList <$> arbitrary
  where convert = bimap Strict.pack Strict.pack

genOpsForMap :: Map ByteString ByteString -> Gen [Op]
genOpsForMap m = do
  inserts <- forM (toList m) $ \(k, v) -> do
    ops <- genOpsForKey k
    return $ ops ++ [Insert k v]
  -- Extra keys (which we delete as the last action)
  extra_keys <- listOf $ arbitrary `suchThat` (`Map.notMember` m)
  deletes    <- forM extra_keys $ \k -> do
    ops <- genOpsForKey k
    return $ ops ++ [Delete k]
  interleaves (inserts ++ deletes)
 where
  genOpsForKey k = listOf (oneof [Insert k <$> arbitrary, pure (Delete k)])

interleaves :: [[a]] -> Gen [a]
interleaves xss = frequency'
  [ (List.length (x : xs), (x :) <$> interleaves (xs : rest))
  | ((x : xs) : rest) <- rots xss
  ]
 where
  frequency' [] = return []
  frequency' xs = frequency xs
  rots xs = List.tail $ List.zipWith (++) (List.tails xs) (List.inits xs)

prop_stateRoot :: Property
prop_stateRoot = forAll genMap $ \m -> forAll (genOpsForMap m)
  $ \ops1 -> forAll (genOpsForMap m) $ \ops2 -> run ops1 === run ops2
 where
  run :: [Op] -> ByteString
  run ops0 = evalState (unMapDB $ initTree >>= go ops0) Map.empty
   where
    go :: [Op] -> RadixTree () -> PureMapDB ByteString
    go []                 t = fromShort . fst <$> merkleizeRadixTree t
    go (Insert k v : ops) t = insertRadixTree k v t >>= go ops
    go (Delete k   : ops) t = deleteRadixTree k t >>= go ops
    go (Lookup _ _ : _  ) _ = error "no lookup in this test please"
    go _                  _ = error "Cannot be reached (GADT pattern match)"

-- | 'createRadixProof' should find the same value as 'lookupRadixTree'
prop_proofLookup :: Property
prop_proofLookup = forAll arbitrary $ \keyStr ->
  let key      = Strict.pack keyStr
      proofRes = lookupProof key
      radixRes = lookupRadix key
  in  isJust proofRes && isJust radixRes && proofRes == radixRes
 where
  run = flip (evalState . unMapDB) Map.empty
  lookupProof k = run $ do
    tree       <- initTree
    tree'      <- insertRadixTree k (mappend k "suffix") tree
    (_, mTree) <- merkleizeRadixTree tree'
    fmap (getValue . fst) <$> createRadixProof k mTree

  lookupRadix k = run $ do
    tree  <- initTree
    tree' <- insertRadixTree k (mappend k "suffix") tree
    fmap fst <$> lookupRadixTree k tree'

prop_proofValid :: Property
prop_proofValid = forAll arbitrary $ \keyPrefixStr ->
  let keyPrefix          = Strict.pack keyPrefixStr
      (key, proof, root) = lookupProof keyPrefix
  in  case proof of
        Nothing -> False
        Just p  -> verifyRadixProof key root p
 where
  run = flip (evalState . unMapDB) Map.empty
  lookupProof kp = run $ do
    tree   <- initTree
    tree'  <- insertRadixTree (mappend kp "1") "body1" tree
    tree'' <- insertRadixTree (mappend kp "12") "body2" tree'
    let key = mappend kp "123"
    tree'''       <- insertRadixTree key "body3" tree''
    (root, mTree) <- merkleizeRadixTree tree'''
    proof         <- fmap fst <$> createRadixProof key mTree
    pure (key, proof, root)

tests :: TestTree
tests = testGroup
  "quickcheck"
  [ testProperty "lookup"                             prop_lookup
  , testProperty "contents"                           prop_stateRoot
  , testProperty "createRadixProof ~ lookupRadixTree" prop_proofLookup
  , testProperty "createRadixProofs are always valid" prop_proofValid
  ]

initTree :: PureMapDB (RadixTree ())
initTree = createRadixTree 2028 Nothing ()
