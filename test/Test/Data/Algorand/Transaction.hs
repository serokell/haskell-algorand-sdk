-- SPDX-FileCopyrightText: 2021 Serokell <https://serokell.io>
--
-- SPDX-License-Identifier: MPL-2.0

{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Test.Data.Algorand.Transaction
  ( unit_simple

  , hprop_canonical_encode_decode
  , hprop_json_encode_decode
  ) where


import Data.Aeson (fromJSON, toJSON)
import Data.ByteString.Base64 (decodeBase64)
import Data.ByteString.Lazy (toStrict)
import Data.MessagePack (pack, unpack)
import Hedgehog (Property, forAll, property, tripping)
import Test.Tasty.HUnit (Assertion, (@?=))

import Data.Algorand.MessagePack (Canonical (..), EitherError)
import Data.Algorand.Transaction (Transaction (..), TransactionType (..))

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
