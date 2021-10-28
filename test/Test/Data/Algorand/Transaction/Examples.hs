-- SPDX-FileCopyrightText: 2019-2020 Algorand
-- SPDX-FileCopyrightText: 2021 Serokell <https://serokell.io/>
--
-- SPDX-License-Identifier: LicenseRef-MIT-Algorand

{-# OPTIONS_GHC -Wno-incomplete-record-updates #-}

module Test.Data.Algorand.Transaction.Examples
  ( example_21

  , unit_example_21
  , unit_example_21_encoding
  , unit_example_21_id
  , unit_example_22
  , unit_example_23
  ) where

import Data.ByteString (ByteString)
import Data.ByteString.Lazy (fromStrict)
import Data.MessagePack (pack, unpack)
import Test.Tasty.HUnit (Assertion, assertFailure, (@?=))

import Data.Algorand.MessagePack (Canonical (Canonical, unCanonical))
import Data.Algorand.Transaction (OnComplete (..), StateSchema (..), Transaction (..),
                                  TransactionType (..), transactionId)
import Data.Algorand.Transaction.Signed (getUnverifiedTransaction, verifyTransaction)

import Test.Domain (genesisHash, sender)
import Test.Util (decodeExpected)

try_example :: Transaction -> ByteString -> Assertion
try_example tx encoded = do
  signed <- unCanonical <$> (unpack . fromStrict $ encoded)
  getUnverifiedTransaction signed @?= tx
  case verifyTransaction signed of
    Nothing -> assertFailure "Signature verification failed"
    Just tx' -> tx' @?= tx

-- Examples are numbered using the line numbers in the original file.
-- Only “call” transactions are tested here for now.
example_21 :: Transaction
example_21 = Transaction
  { tSender = sender
  , tFee = 1234
  , tFirstValid = 9000
  , tLastValid = 9010
  , tNote = Nothing
  , tGenesisId = Nothing
  , tGenesisHash = genesisHash

  , tTxType = ApplicationCallTransaction
    { actApplicationId = 100
    , actOnComplete = OnCompleteNoOp
    , actAccounts = ["AAVDEAJ3NIYOG7XCRBKCJ3T5PUCVL2XASOP3NGX4NPPZ3UX6477PBG6E4Q", "AADQIC4PMKRTFMHAAXYAFSGAUULDI2ABBIYVQJ6GZ5JHY6DJPHTU2SPHYM"]
    , actApprovalProgram = Nothing
    , actAppArguments = ["test"]
    , actClearStateProgram = Nothing
    , actForeignApps = [5555, 6666]
    , actForeignAssets = []
    , actGlobalStateSchema = Just $ StateSchema 0 0
    , actLocalStateSchema = Just $ StateSchema 0 0
    }
  , tGroup = Nothing
  , tLease = Nothing
  , tRekeyTo = Nothing
  }

-- Extra test just for this first transaction.
unit_example_21_encoding :: Assertion
unit_example_21_encoding =
    pack (Canonical example_21) @?= fromStrict expected
  where
    -- This was generated manually using Python SDK
    expected = decodeExpected
      "iqRhcGFhkcQEdGVzdKRhcGF0ksQgACoyATtqMON+4ohUJO59fQVV6uCTn7aa/GvfndL+5/7EIAAHBAuPYqMysOAF8ALIwKUWNGgBCjFYJ8bPUnx4aXnnpGFwZmGSzRWzzRoKpGFwaWRko2ZlZc0E0qJmds0jKKJnaMQgMf0h6zjkEIEZPtNM3zsrg+iHQFS0fZxhgr7w35I464OibHbNIzKjc25kxCAJ+9J2LAj4bFrmv23Xp6kB3mZ111Dgfoxcdphkfbbh/aR0eXBlpGFwcGw="

-- Another extra test.
unit_example_21_id :: Assertion
unit_example_21_id =
  transactionId example_21 @?= "2YIWKLZ2ZOJLINMGNKX3YIT67PUPVMIL72L3VBLDNJVEQAA6ZCWQ"

unit_example_21 :: Assertion
unit_example_21 = try_example example_21 expected
  where
    expected = decodeExpected
      "gqNzaWfEQJIHH5XZSlICTW3xOEHpnQfguLb9dhNjnPZay9UGT8FZr+ig6XjnBher3QR4HDZmwg+3Ei5bPySgTb+5yVLRhwOjdHhuiqRhcGFhkcQEdGVzdKRhcGF0ksQgACoyATtqMON+4ohUJO59fQVV6uCTn7aa/GvfndL+5/7EIAAHBAuPYqMysOAF8ALIwKUWNGgBCjFYJ8bPUnx4aXnnpGFwZmGSzRWzzRoKpGFwaWRko2ZlZc0E0qJmds0jKKJnaMQgMf0h6zjkEIEZPtNM3zsrg+iHQFS0fZxhgr7w35I464OibHbNIzKjc25kxCAJ+9J2LAj4bFrmv23Xp6kB3mZ111Dgfoxcdphkfbbh/aR0eXBlpGFwcGw="

example_22 :: Transaction
example_22 = example_21
  { tTxType = (tTxType example_21)
    { actAccounts = []
    , actForeignApps = []
    }
  }

unit_example_22 :: Assertion
unit_example_22 = try_example example_22 expected
  where
    expected = decodeExpected
      "gqNzaWfEQIucoXp/ThFO4/An6dmp4oYVPojvyZZWYqn7nkKNfnm6qd/TXeu3qiPTEwrTEmhtU5qLGF3Ch+iDZeI6RmH24wujdHhuiKRhcGFhkcQEdGVzdKRhcGlkZKNmZWXNBNKiZnbNIyiiZ2jEIDH9Ies45BCBGT7TTN87K4Poh0BUtH2cYYK+8N+SOOuDomx2zSMyo3NuZMQgCfvSdiwI+Gxa5r9t16epAd5mdddQ4H6MXHaYZH224f2kdHlwZaRhcHBs"

example_23 :: Transaction
example_23 = example_21
  { tTxType = (tTxType example_21)
    { actForeignAssets = [7777, 8888]
    }
  }

unit_example_23 :: Assertion
unit_example_23 = try_example example_23 expected
  where
    expected = decodeExpected
      "gqNzaWfEQEbhKgBScIWg4Cq9jLfSIE+LvH4hJSVGfU6ikR75waHFgIOy1Ut2dwdvkumHuiGzvJ0O0/ouMxnycqCyW49rWw6jdHhui6RhcGFhkcQEdGVzdKRhcGFzks0eYc0iuKRhcGF0ksQgACoyATtqMON+4ohUJO59fQVV6uCTn7aa/GvfndL+5/7EIAAHBAuPYqMysOAF8ALIwKUWNGgBCjFYJ8bPUnx4aXnnpGFwZmGSzRWzzRoKpGFwaWRko2ZlZc0E0qJmds0jKKJnaMQgMf0h6zjkEIEZPtNM3zsrg+iHQFS0fZxhgr7w35I464OibHbNIzKjc25kxCAJ+9J2LAj4bFrmv23Xp6kB3mZ111Dgfoxcdphkfbbh/aR0eXBlpGFwcGw="
