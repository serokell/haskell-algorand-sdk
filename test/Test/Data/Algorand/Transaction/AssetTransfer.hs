-- SPDX-FileCopyrightText: 2021 Serokell <https://serokell.io>
--
-- SPDX-License-Identifier: MPL-2.0

{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Test.Data.Algorand.Transaction.AssetTransfer
  ( unit_encoding
  ) where

import Data.ByteString.Lazy (fromStrict)
import Data.MessagePack (pack)
import Test.Tasty.HUnit (Assertion, (@?=))

import Data.Algorand.MessagePack (Canonical (Canonical))
import Data.Algorand.Transaction (Transaction (..), TransactionType (..))

import Test.Domain (genesisHash)
import Test.Util (decodeExpected)

-- These were generated using the Python SDK.
example :: Transaction
example = Transaction
  { tSender = "6TYVTN4MHDI7PGOXEZZRUIPPCERHSZLEAJUO5RHLGTFBLDHEGK35CYS3HE"
  , tFee = 1234
  , tFirstValid = 9000
  , tLastValid = 9010
  , tNote = Nothing
  , tGenesisId = Just "unit-test"
  , tGenesisHash = genesisHash

  , tTxType = AssetTransferTransaction
    { attXferAsset = 1234
    , attAssetAmount = 1234567
    , attAssetSender = Nothing
    , attAssetReceiver = "OKL27YOBRCNNVQDQQ6UFC6IX7Z4BJ7EPBS3TSSAZ6V54SKYTTCMKUFCOIA"
    , attAssetCloseTo = Nothing
    }

  , tGroup = Nothing
  , tLease = Nothing
  , tRekeyTo = Nothing
  }

unit_encoding :: Assertion
unit_encoding = pack (Canonical example) @?= fromStrict expected
  where
    expected = decodeExpected
      "iqRhYW10zgAS1oekYXJjdsQgcpev4cGImtrAcIeoUXkX/ngU/I8MtzlIGfV7ySsTmJijZmVlzQTSomZ2zSMoo2dlbql1bml0LXRlc3SiZ2jEIDH9Ies45BCBGT7TTN87K4Poh0BUtH2cYYK+8N+SOOuDomx2zSMyo3NuZMQg9PFZt4w40feZ1yZzGiHvESJ5ZWQCaO7E6zTKFYzkMrekdHlwZaVheGZlcqR4YWlkzQTS"
