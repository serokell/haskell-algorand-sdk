-- SPDX-FileCopyrightText: 2019-2020 Algorand
-- SPDX-FileCopyrightText: 2021 Serokell <https://serokell.io/>
--
-- SPDX-License-Identifier: LicenseRef-MIT-Algorand

{-# OPTIONS_GHC -Wno-incomplete-uni-patterns -Wno-incomplete-record-updates #-}

module Test.Data.Algorand.Transaction.Examples where

import Data.ByteArray (convert)
import Data.ByteArray.Sized (sizedByteArray)
import Data.ByteString (ByteString)
import Data.ByteString.Base64 (decodeBase64)
import Data.ByteString.Lazy (fromStrict)
import Data.MessagePack (pack, unpack)
import Test.Tasty.HUnit (Assertion, (@?=), assertFailure)

import Data.Algorand.Address (Address)
import Data.Algorand.MessagePack (Canonical (Canonical, unCanonical))
import Data.Algorand.Transaction (GenesisHash, StateSchema (..), Transaction (..),
                                  TransactionType (..), onCompleteNoOp, transactionId)
import Data.Algorand.Transaction.Signed (getUnverifiedTransaction, verifyTransaction)


sender :: Address
sender = "BH55E5RMBD4GYWXGX5W5PJ5JAHPGM5OXKDQH5DC4O2MGI7NW4H6VOE4CP4"

genesisHash :: GenesisHash
genesisHash = genesisHashFromBytes "Mf0h6zjkEIEZPtNM3zsrg+iHQFS0fZxhgr7w35I464M="


{-
 - Examples are numbered using the line numbers in the original file.
 - Only “call” transactions are tested here for now.
 -
 -}

example_21 :: Transaction
example_21 = Transaction
  { tSender = sender
  , tFee = 1234
  , tFirstValid = 9000
  , tLastValid = 9010
  , tNote = Nothing
  , tGenesisId = Nothing
  , tGenesisHash = Just genesisHash

  , tTxType = ApplicationCallTransaction
    { actApplicationId = 100
    , actOnComplete = onCompleteNoOp
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
    pack (Canonical example_21) @?= fromStrict example_21_encoded
  where
    -- This was generated manually using Python SDK
    example_21_encoded :: ByteString
    example_21_encoded = result
      "i6RhcGFhkcQEdGVzdKRhcGFupGNhbGykYXBhdJLEIAAqMgE7ajDjfuKIVCTufX0FVergk5+2mvxr353S/uf+xCAABwQLj2KjMrDgBfACyMClFjRoAQoxWCfGz1J8eGl556RhcGZhks0Vs80aCqRhcGlkZKNmZWXNBNKiZnbNIyiiZ2jEIDH9Ies45BCBGT7TTN87K4Poh0BUtH2cYYK+8N+SOOuDomx2zSMyo3NuZMQgCfvSdiwI+Gxa5r9t16epAd5mdddQ4H6MXHaYZH224f2kdHlwZaRhcHBs"

-- Another extra test.
unit_example_21_id :: Assertion
unit_example_21_id =
  transactionId example_21 @?= "C7HZ22XI4N33LDXG7UXIIMDJBINOBBE6A3D766EYGHLMRPIIDEZA"

example_21_expected :: ByteString
example_21_expected = result
  "gqNzaWfEQM1gWq1M1e6wevcO9v4qgBkV+AjP/Xu6raBCUcpXKPCS40EWfXjNppNrld08zSuz8GYm5sxPYGmx0Gkbe3E9nAGjdHhui6RhcGFhkcQEdGVzdKRhcGFupGNhbGykYXBhdJLEIAAqMgE7ajDjfuKIVCTufX0FVergk5+2mvxr353S/uf+xCAABwQLj2KjMrDgBfACyMClFjRoAQoxWCfGz1J8eGl556RhcGZhks0Vs80aCqRhcGlkZKNmZWXNBNKiZnbNIyiiZ2jEIDH9Ies45BCBGT7TTN87K4Poh0BUtH2cYYK+8N+SOOuDomx2zSMyo3NuZMQgCfvSdiwI+Gxa5r9t16epAd5mdddQ4H6MXHaYZH224f2kdHlwZaRhcHBs"

unit_example_21 :: Assertion
unit_example_21 = try_example example_21 example_21_expected


example_22 :: Transaction
example_22 = example_21
  { tTxType = (tTxType example_21)
    { actAccounts = []
    , actForeignApps = []
    }
  }

example_22_expected :: ByteString
example_22_expected = result
  "gqNzaWfEQCeQtBiXAu8KJSGwQb5bVV3SxkjRQwxi38SmwXK6nDxcnOtMIwJlaOBYc61N6Bh9/H9rwK2n6xAWJpCbaxx1MAujdHhuiaRhcGFhkcQEdGVzdKRhcGFupGNhbGykYXBpZGSjZmVlzQTSomZ2zSMoomdoxCAx/SHrOOQQgRk+00zfOyuD6IdAVLR9nGGCvvDfkjjrg6Jsds0jMqNzbmTEIAn70nYsCPhsWua/bdenqQHeZnXXUOB+jFx2mGR9tuH9pHR5cGWkYXBwbA=="

unit_example_22 :: Assertion
unit_example_22 = try_example example_22 example_22_expected


example_23 :: Transaction
example_23 = example_21
  { tTxType = (tTxType example_21)
    { actForeignAssets = [7777, 8888]
    }
  }

example_23_expected :: ByteString
example_23_expected = result
  "gqNzaWfEQM/gZeGvub0ZnFe2ei++W3WZQIbhxNbBWweREWBZMPtPNE/kzRpWz2g3z3gniJweLTUo6LDK18VAd6QYSzc/3g+jdHhujKRhcGFhkcQEdGVzdKRhcGFupGNhbGykYXBhc5LNHmHNIrikYXBhdJLEIAAqMgE7ajDjfuKIVCTufX0FVergk5+2mvxr353S/uf+xCAABwQLj2KjMrDgBfACyMClFjRoAQoxWCfGz1J8eGl556RhcGZhks0Vs80aCqRhcGlkZKNmZWXNBNKiZnbNIyiiZ2jEIDH9Ies45BCBGT7TTN87K4Poh0BUtH2cYYK+8N+SOOuDomx2zSMyo3NuZMQgCfvSdiwI+Gxa5r9t16epAd5mdddQ4H6MXHaYZH224f2kdHlwZaRhcHBs"

unit_example_23 :: Assertion
unit_example_23 = try_example example_23 example_23_expected



{-
 - Testing utils
 -}

try_example :: Transaction -> ByteString -> Assertion
try_example tx encoded = do
  signed <- unCanonical <$> (unpack . fromStrict $ encoded)
  getUnverifiedTransaction signed @?= tx
  case verifyTransaction signed of
    Nothing -> assertFailure "Signature verification failed"
    Just tx' -> tx' @?= tx


{-
 - Decoding utils
 -}

genesisHashFromBytes :: ByteString -> GenesisHash
genesisHashFromBytes t =
  let
    Right bs = decodeBase64 t
    Just h = sizedByteArray (convert bs)
  in h

result :: ByteString -> ByteString
result = (\(Right r) -> r) . decodeBase64
