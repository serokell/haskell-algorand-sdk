-- SPDX-FileCopyrightText: 2021 Serokell <https://serokell.io>
--
-- SPDX-License-Identifier: MPL-2.0

{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Test.Data.Algorand.Transaction.Pay
  ( unit_encoding
  ) where

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
  { tSender = "6TYVTN4MHDI7PGOXEZZRUIPPCERHSZLEAJUO5RHLGTFBLDHEGK35CYS3HE"
  , tFee = 1234
  , tFirstValid = 9000
  , tLastValid = 9010
  , tNote = Nothing
  , tGenesisId = Just "unit-test"
  , tGenesisHash = Just $ genesisHashFromBytes "Mf0h6zjkEIEZPtNM3zsrg+iHQFS0fZxhgr7w35I464M="

  , tTxType = PaymentTransaction
    { ptReceiver = "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAY5HFKQ"
    , ptAmount = 1234567
    , ptCloseRemainderTo = Nothing
    }

  , tGroup = Nothing
  , tLease = Nothing
  , tRekeyTo = Nothing
  }

example_encoded :: ByteString
example_encoded = result
   "iKNhbXTOABLWh6NmZWXNBNKiZnbNIyijZ2VuqXVuaXQtdGVzdKJnaMQgMf0h6zjkEIEZPtNM3zsrg+iHQFS0fZxhgr7w35I464OibHbNIzKjc25kxCD08Vm3jDjR95nXJnMaIe8RInllZAJo7sTrNMoVjOQyt6R0eXBlo3BheQ=="

unit_encoding :: Assertion
unit_encoding =
  pack (Canonical example) @?= fromStrict example_encoded
