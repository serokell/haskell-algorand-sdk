-- SPDX-FileCopyrightText: 2021 Serokell <https://serokell.io>
--
-- SPDX-License-Identifier: MPL-2.0

module Test.Gen
  ( genSecretKeyBytes
  , genProgram
  , genPublicKey
  , genSigner
  , genTransaction
  ) where

import qualified Hedgehog.Gen as G
import qualified Hedgehog.Range as R

import Data.ByteArray (ByteArrayAccess, Bytes, convert)
import Data.ByteArray.Sized (SizedByteArray, sizedByteArray)
import Data.ByteString (ByteString)
import Data.Proxy (Proxy (Proxy))
import GHC.TypeLits (KnownNat, natVal)
import Hedgehog (MonadGen)

import Crypto.Algorand.Key (PublicKey, pkFromBytes, pkSize, skFromBytes, skSize)
import Data.Algorand.Address (Address, fromPublicKey)
import Data.Algorand.Amount (microAlgos)
import Data.Algorand.Round (Round (..))
import Data.Algorand.Transaction (OnComplete (..), StateSchema (..), Transaction (..),
                                  TransactionType (..))

import Test.Domain (Signer (..))

-- | Generate a compiled TEAL program.
--
-- FIXME: Currently, this just generates arbitrary (non-empty) bytes.
genProgram :: MonadGen m => m ByteString
genProgram = G.bytes (R.linear 1 1000)

-- | Generate raw bytes for a random 'SecretKey'.
genSecretKeyBytes :: MonadGen m => m ByteString
genSecretKeyBytes = do
  bs <- G.bytes (R.singleton skSize)
  case skFromBytes bs of
    Nothing -> G.discard
    Just _ -> pure bs

-- | Generate a random 'PublicKey'.
genPublicKey :: MonadGen m => m PublicKey
genPublicKey = G.justT (pkFromBytes <$> G.bytes (R.singleton pkSize))

-- | Generate a random 'Address'.
genAddress :: MonadGen m => m Address
genAddress = fromPublicKey <$> genPublicKey

-- | Generate a random bytes.
genBytes :: MonadGen m => R.Range Int -> m Bytes
genBytes r = convert <$> G.bytes r

-- | Generate a random 'Signer'.
genSigner :: MonadGen m => m Signer
genSigner = G.choice
    [ SignerSimple <$> G.justT (skFromBytes <$> genSecretKeyBytes)
    , SignerContract <$> genProgram
    ]

genSizedBytes
  :: forall m n bs. (MonadGen m, KnownNat n, ByteArrayAccess bs)
  => (R.Range Int -> m bs) -> m (SizedByteArray n bs)
genSizedBytes gen = G.justT (sizedByteArray <$> gen (R.singleton size))
  where
    size = fromIntegral $ natVal (Proxy @n)

-- | Generate a random 'StateSchema'.
genStateSchema :: MonadGen m => m StateSchema
genStateSchema = StateSchema <$> G.word64 (R.linear 0 1000) <*> G.word64 (R.linear 0 1000)

-- | Generate a random 'OnComplete'.
genOnComplete :: MonadGen m => m OnComplete
genOnComplete = G.enumBounded

-- | Generate a random 'TransactionType'.
genTransactionType :: MonadGen m => m TransactionType
genTransactionType = G.choice
  [ PaymentTransaction
    <$> genAddress
    <*> G.integral R.linearBounded
    <*> G.maybe genAddress
  , ApplicationCallTransaction
    <$> G.word64 R.constantBounded
    <*> genOnComplete
    <*> G.list (R.linear 0 10) genAddress
    <*> G.maybe (G.bytes $ R.linear 1 100)  -- cannot be empty
    <*> G.list (R.linear 0 10) (G.bytes (R.linear 0 32))
    <*> G.maybe (G.bytes $ R.linear 1 100)  -- cannot be empty
    <*> G.list (R.linear 0 10) (G.word64 (R.linear 0 10000))
    <*> G.list (R.linear 0 10) (G.word64 (R.linear 0 10000))
    <*> (Just <$> genStateSchema)
    <*> (Just <$> genStateSchema)
  , AssetTransferTransaction
    <$> G.word64 R.constantBounded
    <*> genAmount
    <*> G.maybe genAddress
    <*> genAddress
    <*> G.maybe genAddress
  ]
  where
    genAmount = microAlgos <$> G.word64 R.constantBounded

-- | Generate a random 'Transaction'.
genTransaction :: MonadGen m => m Transaction
genTransaction =
  Transaction
  <$> genAddress
  <*> G.integral R.linearBounded
  <*> genRound
  <*> genRound
  <*> G.maybe (G.bytes $ R.linear 1 100)  -- cannot be empty
  <*> G.maybe (G.text (R.singleton 44) G.unicode)
  <*> G.maybe (genSizedBytes genBytes)
  <*> genTransactionType
  <*> G.maybe (genSizedBytes genBytes)
  <*> G.maybe (genSizedBytes genBytes)
  <*> G.maybe genAddress
  where
    genRound = Round <$> G.word64 R.constantBounded
