-- SPDX-FileCopyrightText: 2021 Serokell <https://serokell.io>
--
-- SPDX-License-Identifier: MPL-2.0

module Test.Network.Algorand.Api.Node
  ( test_VersionCall
  , test_TransactionsParams
  , test_CompileTeal
  ) where

import qualified Data.Text as T

import System.Directory (listDirectory)
import System.FilePath (dropExtension, (</>))
import Test.Tasty (TestTree)
import Test.Tasty.HUnit (testCase)

import qualified Network.Algorand.Api as Api

import Data.Algorand.Teal (TealCompilationResult)
import Network.Algorand.Api.Node (SuggestedParams (spGenesisHash), Version (vGenesisId))

import Test.Util (goldenTest, nodeClient)

resourcesNode :: FilePath
resourcesNode = "test/resources/node"

test_VersionCall :: IO [TestTree]
test_VersionCall = listDirectory dir >>= mapM runTest
  where
    dir = resourcesNode </> "versions"
    runTest file = return . testCase (dir </> file) $ do
      api <- Api._version <$> nodeClient
      api >>= goldenTest @Version (dir </> file) vGenesisId

-- TODO:
-- test_Transactions
-- test_TransactionsRaw
-- test_TransactionsPending

test_TransactionsParams :: IO [TestTree]
test_TransactionsParams = listDirectory dir >>= mapM runTest
  where
    dir = resourcesNode </> "transactions" </> "params"
    runTest file = return . testCase (dir </> file) $ do
      api <- Api._transactionsParams <$> nodeClient
      api >>= goldenTest @SuggestedParams (dir </> file) spGenesisHash

test_CompileTeal :: IO [TestTree]
test_CompileTeal = listDirectory dir >>= mapM runTest
  where
    dir = resourcesNode </> "teal" </> "compile"
    runTest file = return . testCase (dir </> file) $ do
      api <- Api._compileTeal <$> nodeClient
      let code = T.pack $ dropExtension file
      api code >>= goldenTest @TealCompilationResult (dir </> file) id
