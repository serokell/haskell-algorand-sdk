-- SPDX-FileCopyrightText: 2021 Serokell <https://serokell.io>
--
-- SPDX-License-Identifier: MPL-2.0

module Test.Network.Algorand.Api.Indexer
  ( test_AccountCall
  , test_BlockCall
  , test_HealthCall
  ) where

import qualified Data.Text as T

import Data.List (stripPrefix)
import System.Directory (listDirectory)
import System.FilePath ((</>))
import System.FilePath.Posix (dropExtension)
import Test.Tasty (TestTree)
import Test.Tasty.HUnit (assertFailure, testCase)
import Text.Read (readMaybe)

import qualified Network.Algorand.Api as Api

import Data.Algorand.Address (fromText)
import Data.Algorand.Round (Round (..))
import Network.Algorand.Api.Indexer (BlockResp, Health (..), IdxAccountResponse (iarAccount))

import Test.Util (goldenTest, idxClient, split)

resourcesIndexer :: FilePath
resourcesIndexer = "test/resources/indexer"

test_AccountCall :: IO [TestTree]
test_AccountCall = listDirectory dir >>= mapM runTest
  where
    dir = resourcesIndexer </> "accounts"
    runTest file = return . testCase (dir </> file) $ do
      api <- Api._accountIdx <$> idxClient
      let (a, r) = split '_' $ dropExtension file
      case (,) <$> fromText (T.pack a) <*> (Round <$> readMaybe r) of
        Just (adr, rnd) -> do
          api adr (Just rnd) >>=
            goldenTest @IdxAccountResponse (dir </> file) iarAccount
        Nothing -> assertFailure $
          "Can't get address and round from `" <> dir </> file <> "`, file \
          \name must be in form: `ADDRESS_ROUND.json`, where ADDRESS is valid \
          \algo readable address and ROUND is valid Word64 value"

test_BlockCall :: IO [TestTree]
test_BlockCall = listDirectory dir >>= mapM runTest
  where
    dir = resourcesIndexer </> "blocks"
    runTest file = return . testCase (dir </> file) $ do
      api <- Api._blockIdx <$> idxClient
      case (stripPrefix "block" $ dropExtension file) >>= readMaybe of
        Just r -> do
          api (Round r) >>= goldenTest @BlockResp (dir </> file) id
        Nothing -> assertFailure $
          "Can't get round from `" <> dir </> file <> "`, file name must be in \
          \form: `blockROUND.json`, where ROUND is valid Word64 value"

test_HealthCall :: IO [TestTree]
test_HealthCall = listDirectory dir >>= mapM runTest
  where
    dir = resourcesIndexer </> "health"
    runTest file = return . testCase (dir </> file) $ do
      api <- Api._health <$> idxClient
      let constantFields Health{..} = (hDbAvailable, hVersion)
      api >>= goldenTest @Health (dir </> file) constantFields
