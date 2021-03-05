-- SPDX-FileCopyrightText: 2021 Serokell <https://serokell.io>
--
-- SPDX-License-Identifier: MPL-2.0

{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Test.Data.Algorand.Transaction.Pay where

import Data.ByteString (ByteString)
import Data.ByteString.Lazy (fromStrict)
import Data.MessagePack (pack)

import Test.Tasty.HUnit (Assertion, (@?=))

import Data.Algorand.MessagePack (Canonical (Canonical))
import Data.Algorand.Transaction (Transaction (..), TransactionType (..))

import Test.Data.Algorand.Transaction.Examples (genesisHashFromBytes, result)


-- These were generated using the Python SDK.

example :: Transaction
example = Transaction
  { tSender = "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAY5HFKQ"
  , tFee = 1234
  , tFirstValid = 9000
  , tLastValid = 9010
  , tNote = Nothing
  , tGenesisId = Just "unit-test"
  , tGenesisHash = genesisHashFromBytes "Mf0h6zjkEIEZPtNM3zsrg+iHQFS0fZxhgr7w35I464M="

  , tTxType = PaymentTransaction
    { ptReceiver = "OKL27YOBRCNNVQDQQ6UFC6IX7Z4BJ7EPBS3TSSAZ6V54SKYTTCMKUFCOIA"
    , ptAmount = 1234567
    , ptCloseRemainderTo = Nothing
    }

  , tGroup = Nothing
  , tLease = Nothing
  , tRekeyTo = Nothing
  }

example_encoded :: ByteString
example_encoded = result
   "iaNhbXTOABLWh6NmZWXNBNKiZnbNIyijZ2VuqXVuaXQtdGVzdKJnaMQgMf0h6zjkEIEZPtNM3zsrg+iHQFS0fZxhgr7w35I464OibHbNIzKjcmN2xCByl6/hwYia2sBwh6hReRf+eBT8jwy3OUgZ9XvJKxOYmKNzbmTEIAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAApHR5cGWjcGF5"

unit_encoding :: Assertion
unit_encoding =
  pack (Canonical example) @?= fromStrict example_encoded
