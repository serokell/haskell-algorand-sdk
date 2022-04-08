-- SPDX-FileCopyrightText: 2021 Serokell <https://serokell.io>
--
-- SPDX-License-Identifier: MPL-2.0

{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Test.Data.Algorand.Transaction
  ( unit_simple

  , hprop_canonical_encode_decode
  , hprop_json_encode_decode

  , test_transactions_group_validation
  ) where


import Control.Monad (forM_)
import Data.Aeson (fromJSON, toJSON)
import qualified Data.ByteArray as BA
import Data.ByteString.Base64 (decodeBase64, encodeBase64)
import Data.ByteString.Lazy (toStrict)
import qualified Data.Map as Map
import Data.MessagePack (pack, unpack)
import qualified Data.Text.Encoding as T
import Hedgehog (Property, forAll, property, tripping)
import Test.Tasty (TestTree)
import Test.Tasty.HUnit (Assertion, assertBool, testCaseSteps, (@?=))
import Test.Util (idxClient)

import Data.Algorand.MessagePack (Canonical (..), EitherError)
import Data.Algorand.Round
import Data.Algorand.Transaction
import Data.Algorand.Transaction.Group
import Network.Algorand.Api.Indexer (TransactionResp (..))
import qualified Network.Algorand.Api.Indexer as IdxApi

import Test.Domain (genesisHash, sender)
import Test.Gen (genTransaction)

unit_simple :: Assertion
unit_simple = do
    toStrict (pack $ Canonical txn) @?= encoded_bytes
  where
    txn :: Transaction
    txn = Transaction
      { tSender = sender
      , tFee = 1000
      , tFirstValid = 5000
      , tLastValid = 5100
      , tNote = Nothing
      , tGenesisId = Nothing
      , tGenesisHash = genesisHash

      , tTxType = PaymentTransaction
        { ptReceiver = sender
        , ptAmount = 500
        , ptCloseRemainderTo = Nothing
        }
      , tGroup = Nothing
      , tLease = Nothing
      , tRekeyTo = Nothing
      }

    encoded = "iKNhbXTNAfSjZmVlzQPoomZ2zROIomdoxCAx/SHrOOQQgRk+00zfOyuD6IdAVLR9nGGCvvDfkjjrg6Jsds0T7KNyY3bEIAn70nYsCPhsWua/bdenqQHeZnXXUOB+jFx2mGR9tuH9o3NuZMQgCfvSdiwI+Gxa5r9t16epAd5mdddQ4H6MXHaYZH224f2kdHlwZaNwYXk="
    Right encoded_bytes = decodeBase64 encoded

hprop_canonical_encode_decode :: Property
hprop_canonical_encode_decode = property $ do
  tx <- forAll genTransaction
  tripping tx (pack . Canonical) (fmap unCanonical . unpack @EitherError)

hprop_json_encode_decode :: Property
hprop_json_encode_decode = property $ do
  tx <- forAll genTransaction
  tripping tx toJSON fromJSON

test_transactions_group_validation :: [TestTree]
test_transactions_group_validation =
  flip map
    [ 0         -- just as an example
    , 17608925  -- many txs in block

    -- TODO: these do not work
    -- , 17608932  -- has Config tx type
    -- , 20853753, 20855687, 20856192  -- were not working at some moment
    -- , 20856264  -- has Close tx type
    ] $ \rid ->
    testCaseSteps ("Round " <> show rid) $ \step -> do
      cli <- idxClient
      IdxApi.BlockResp
        { brTransactions = txResps
        , brGenesisId = gId
        , brGenesisHash = gHash
        } <- IdxApi._blockIdx cli (Round rid)

      -- let txResps = fromMaybe (error "Unexpectedly no txs") mTxResps
      let txs = IdxApi.transactionRespToTransaction gId gHash <$> txResps
      let (_, groupedTxs) = splitTransactionsByGroup txs

      -- Tx groups evaluation relies on 'transactionId', so we better check it too
      forM_ (zip txResps txs) $ \(txResp, tx) ->
        assertBool ("Transaction id is evaluated incorrectly for tx " <> show (trId txResp))
          (trId txResp == transactionId tx)

      forM_ (Map.toList groupedTxs) $ \(groupId, txsInGroup) -> do
        -- TODO: have normal newtype for group id with Buildable instance
        step $ "Checking group " <> show (T.encodeUtf8 (encodeBase64 $ BA.convert groupId))

        assertBool "Group id is calculated incorrectly" $
          (getGroupIdFor txsInGroup == groupId)

        assertBool "Real group is considered invalid" $
          isValidGroup txsInGroup
