-- SPDX-FileCopyrightText: 2021 Serokell <https://serokell.io>
--
-- SPDX-License-Identifier: MPL-2.0

module Test.Network.Algorand.Indexer.Golden
  ( unit_IdxAccountResponse
  , unit_BlockResp
  , unit_Health
  ) where

import Data.Aeson (FromJSON, eitherDecodeFileStrict')
import System.Directory (listDirectory)
import System.FilePath ((</>))
import Test.Tasty.HUnit (Assertion, assertFailure)

import Network.Algorand.Api.Indexer (BlockResp, Health, IdxAccountResponse)

unit_IdxAccountResponse :: Assertion
unit_IdxAccountResponse =
  goldenTest @IdxAccountResponse (resourcesIndexer </> "accounts")

unit_BlockResp :: Assertion
unit_BlockResp = goldenTest @BlockResp (resourcesIndexer </> "blocks")

unit_Health :: Assertion
unit_Health = goldenTest @Health (resourcesIndexer </> "health")

resourcesIndexer :: FilePath
resourcesIndexer = "test/resources/indexer"

goldenTest :: forall a. FromJSON a => FilePath -> Assertion
goldenTest location = mapM_ singleTest =<< listDirectory location
  where
    singleTest fileName =
      eitherDecodeFileStrict' @a (location </> fileName) >>= \case
        Left err -> assertFailure err
        Right _ -> pure ()
