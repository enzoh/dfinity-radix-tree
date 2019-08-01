{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS -Wall #-}

module Units
  ( tests
  )
where

import Control.Arrow
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.State.Strict
import Data.Aeson
import Data.ByteString.Char8      as Strict
import Data.ByteString.Lazy.Char8 as Lazy
import Data.ByteString.Short
import Data.HashMap.Strict
import Data.List                  as List
import Data.Maybe
import Test.Tasty
import Test.Tasty.HUnit

import DFINITY.RadixTree
import Types

tests :: IO TestTree
tests = do
  contents <- Lazy.readFile "test/tests.json"
  vectors  <- either fail return $ eitherDecode contents
  pure $ testGroup "units"
                   [ testCase name $ run ops | (name, ops) <- toList vectors ]

run :: [Op] -> IO ()
run ops = void $ flip (runStateT . unMapDB) mempty $ do
  tree <- createRadixTree 2048 Nothing ()
  foldM_ step tree ops

step :: RadixTree () -> Op -> IOMapDB (RadixTree ())
step tree op = do
  liftIO $ print op
  case op of
    Insert key value -> do
      tree' <- insertRadixTree key value tree
      printRadixTree tree'
      pure tree'
    Delete key -> do
      tree' <- deleteRadixTree key tree
      printRadixTree tree'
      pure tree'
    Lookup key value -> do
      result <- lookupRadixTree key tree
      case result of
        Nothing | isNothing value -> pure tree
        Nothing                   -> throw
          ["Expecting value ", ", but received no value for key "]
          [fromJust value, key]
        Just (value', tree') | value == Just value' -> pure tree'
        Just (value', _)                            -> throw
          ["Expecting value ", ", but received value ", " for key "]
          [fromMaybe "null" value, value', key]
    Merkleize Nothing -> do
      (_, tree') <- first fromShort <$> merkleizeRadixTree tree
      pure tree'
    Merkleize (Just value) -> do
      (value', tree') <- first fromShort <$> merkleizeRadixTree tree
      if value == value'
        then pure tree'
        else throw ["Expecting state root ", ", but received state root "]
                   [value, value']

throw :: MonadIO m => [String] -> [Strict.ByteString] -> m a
throw err =
  liftIO
    . assertFailure
    . List.concat
    . List.zipWith mappend err
    . List.map show
