-- SPDX-FileCopyrightText: 2021 Serokell <https://serokell.io>
--
-- SPDX-License-Identifier: MPL-2.0

module Test.Util
  ( goldenTest
  , idxClient

  , split
  ) where

import Control.Arrow (second)
import Data.Aeson (FromJSON, eitherDecodeFileStrict')
import Data.Maybe (fromJust)
import Servant.Client.Generic (AsClientT)
import Test.Tasty.HUnit (Assertion, assertFailure, (@?=))

import qualified Network.Algorand.Api as Api

import Network.Algorand.Client (AlgoIndexer (..), connectToIndexer)
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

-- | Splits string by delimiter
split :: Char -> String -> (String, String)
split _ [] = ("", "")
split c xs = second (drop 1) . break (== c) $ xs
