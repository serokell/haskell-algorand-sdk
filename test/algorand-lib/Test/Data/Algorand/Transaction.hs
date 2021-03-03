-- SPDX-FileCopyrightText: 2021 Serokell <https://serokell.io>
--
-- SPDX-License-Identifier: MPL-2.0

{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Test.Data.Algorand.Transaction where

import Data.Aeson (fromJSON, toJSON)
import Data.ByteArray (Bytes, ByteArrayAccess, convert)
import Data.ByteArray.Sized (SizedByteArray, sizedByteArray)
import Data.ByteString.Base64 (decodeBase64)
import Data.ByteString.Lazy (toStrict)
import Data.MessagePack (pack, unpack)
import Data.Proxy (Proxy (Proxy))
import GHC.TypeLits (KnownNat, natVal)

import Hedgehog (MonadGen, Property, forAll, property, tripping)
import qualified Hedgehog.Gen as G
import qualified Hedgehog.Range as R
import Test.Tasty.HUnit (Assertion, (@?=))

import Crypto.Algorand.Signature (skFromBytes, toPublic)
import Data.Algorand.Address (Address, fromPublicKey)
import Data.Algorand.MessagePack (Canonical (Canonical, unCanonical), EitherError)
import Data.Algorand.Transaction (StateSchema (..), Transaction (..), TransactionType (..), signTransaction, verifyTransaction)

import Test.Data.Algorand.Transaction.Examples (genesisHash, sender)
import Test.Crypto.Algorand.Signature (genPublicKey, genSecretKeyBytes)


genAddress :: MonadGen m => m Address
genAddress = fromPublicKey <$> genPublicKey

genBytes :: MonadGen m => R.Range Int -> m Bytes
genBytes r = convert <$> G.bytes r

genSizedBytes
  :: forall m n bs. (MonadGen m, KnownNat n, ByteArrayAccess bs)
  => (R.Range Int -> m bs) -> m (SizedByteArray n bs)
genSizedBytes gen = G.justT (sizedByteArray <$> gen (R.singleton size))
  where
    size = fromIntegral $ natVal (Proxy @n)

genStateSchema :: MonadGen m => m StateSchema
genStateSchema = StateSchema <$> G.word64 (R.linear 0 1000) <*> G.word64 (R.linear 0 1000)

genTransactionType :: MonadGen m => m TransactionType
genTransactionType = G.choice
  [ PaymentTransaction
    <$> genAddress
    <*> G.integral R.linearBounded
    <*> G.maybe genAddress
  , ApplicationCallTransaction
    <$> G.word64 R.constantBounded
    <*> G.word64 R.constantBounded
    <*> G.list (R.linear 0 10) genAddress
    <*> G.maybe (G.bytes $ R.linear 1 100)  -- cannot be empty
    <*> G.list (R.linear 0 10) (G.bytes (R.linear 0 32))
    <*> G.maybe (G.bytes $ R.linear 1 100)  -- cannot be empty
    <*> G.list (R.linear 0 10) (G.word64 (R.linear 0 10000))
    <*> G.list (R.linear 0 10) (G.word64 (R.linear 0 10000))
    <*> (Just <$> genStateSchema)
    <*> (Just <$> genStateSchema)
  ]

genTransaction :: MonadGen m => m Transaction
genTransaction =
  Transaction
  <$> genAddress
  <*> G.integral R.linearBounded
  <*> G.word64 R.constantBounded
  <*> G.word64 R.constantBounded
  <*> G.maybe (G.bytes $ R.linear 1 100)  -- cannot be empty
  <*> G.maybe (G.text (R.singleton 44) G.unicode)
  <*> genSizedBytes genBytes
  <*> genTransactionType
  <*> G.maybe (genSizedBytes genBytes)
  <*> G.maybe (genSizedBytes genBytes)
  <*> G.maybe genAddress


hprop_canonical_encode_decode :: Property
hprop_canonical_encode_decode = property $ do
  tx <- forAll genTransaction
  tripping tx (pack . Canonical) (fmap unCanonical . unpack @EitherError)

hprop_json_encode_decode :: Property
hprop_json_encode_decode = property $ do
  tx <- forAll genTransaction
  tripping tx toJSON fromJSON


hprop_sign_verify :: Property
hprop_sign_verify = property $ do
  Just sk <- skFromBytes <$> forAll genSecretKeyBytes
  tx <- forAll genTransaction
  let tx' = tx { tSender = fromPublicKey (toPublic sk) }
  tripping tx' (signTransaction sk) verifyTransaction


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
