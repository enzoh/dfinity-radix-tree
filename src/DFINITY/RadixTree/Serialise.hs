{-# LANGUAGE LambdaCase #-}

{-# OPTIONS -Wall #-}

module DFINITY.RadixTree.Serialise
  ( decodeLeaf
  , decodeMaybe
  , decodeSide
  , decodeShort
  , encodeMaybe
  , encodeSide
  , encodeShort
  )
where

import Codec.Serialise.Decoding
import Codec.Serialise.Encoding
import Control.Monad
import Data.ByteString
import Data.ByteString.Short

decodeLeaf :: Int -> Decoder s (Maybe ByteString)
decodeLeaf = \case
  3 -> pure Nothing
  4 -> pure <$> decodeBytes
  _ -> fail "decodeLeaf: invalid argument"

decodeMaybe :: Decoder s a -> Decoder s (Maybe a)
decodeMaybe value = peekTokenType >>= \case
  TypeNull -> decodeNull >> pure Nothing
  _        -> pure <$> value

decodeSide :: Decoder s ByteString
decodeSide = void decodeTag >> decodeBytes

decodeShort :: Decoder s ShortByteString
decodeShort = toShort <$> decodeBytes

encodeMaybe :: (a -> Encoding) -> Maybe a -> Encoding
encodeMaybe = maybe encodeNull

encodeSide :: ByteString -> Encoding
encodeSide side = encodeTag 42 <> encodeBytes side

encodeShort :: ShortByteString -> Encoding
encodeShort = encodeBytes . fromShort
