-- SPDX-FileCopyrightText: 2021 Serokell <https://serokell.io>
--
-- SPDX-License-Identifier: MPL-2.0

module Test.Util
  ( decodeExpected
  , goldenTest
  , genesisHashFromBytes

  , idxClient
  , nodeClient

  , split
  ) where

import qualified Data.Text as T

import Control.Arrow (second)
import Data.Aeson (FromJSON, eitherDecodeFileStrict')
import Data.ByteArray (convert)
import Data.ByteArray.Sized (sizedByteArray)
import Data.ByteString (ByteString)
import Data.ByteString.Base64 (decodeBase64)
import Data.Maybe (fromJust)
import Servant.Client.Generic (AsClientT)
import Test.Tasty.HUnit (Assertion, assertFailure, (@?=))

import qualified Network.Algorand.Api as Api

import Data.Algorand.Transaction (GenesisHash (..))
import Network.Algorand.Client (AlgoIndexer (..), AlgoNode (..), connectToIndexer, connectToNode)
import Network.Algorand.Definitions (DefaultHost (..), Network (TestnetV1), getDefaultHost)

-- | Compares result from location with expected one.
-- Transform function must be provided, which applies to both values.
-- In case you do not need transformation, provide `id`
goldenTest
  :: forall a b. (Show b, Eq b, FromJSON a)
  => FilePath -> (a -> b) -> a -> Assertion
goldenTest location transform expected = do
  eitherDecodeFileStrict' @a location >>= \case
    Left err -> assertFailure err
    Right result -> transform result @?= transform expected

-- | Helper to call Indexer API in tests
idxClient :: IO (Api.IndexerApi (AsClientT IO))
idxClient = getAlgoIndexer . snd <$> connectToIndexer host TestnetV1
  where
    host = ahIndexer . fromJust $ getDefaultHost TestnetV1

-- | Helper to call Node API in tests
nodeClient :: IO (Api.NodeApi (AsClientT IO))
nodeClient = getAlgoNode . snd <$> connectToNode host TestnetV1
  where
    host = ahNode . fromJust $ getDefaultHost TestnetV1

-- | Splits string by delimiter
split :: Char -> String -> (String, String)
split _ [] = ("", "")
split c xs = second (drop 1) . break (== c) $ xs

-- | Get 'GenesisHash' from 'ByteString'
genesisHashFromBytes :: ByteString -> Maybe GenesisHash
genesisHashFromBytes t = case decodeBase64 t of
  Right bs -> GenesisHash <$> sizedByteArray (convert bs)
  Left _ -> Nothing

-- | Decode base64 'ByteString'
-- This function is helper for expected values in tests.
-- NOTE: it's unsafe
decodeExpected :: ByteString -> ByteString
decodeExpected t = case decodeBase64 t of
  Right bs -> bs
  Left e -> error $ T.unpack e
