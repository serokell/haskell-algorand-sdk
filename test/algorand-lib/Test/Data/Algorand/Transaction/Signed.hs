-- SPDX-FileCopyrightText: 2021 Serokell <https://serokell.io>
--
-- SPDX-License-Identifier: MPL-2.0

{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Test.Data.Algorand.Transaction.Signed where

import Data.Aeson (fromJSON, toJSON)
import Data.ByteArray (Bytes, ByteArrayAccess, convert)
import Data.ByteArray.Sized (SizedByteArray, sizedByteArray)
import Data.ByteString (ByteString)
import Data.ByteString.Base64 (decodeBase64)
import Data.ByteString.Lazy (toStrict)
import Data.MessagePack (pack, unpack)
import Data.Proxy (Proxy (Proxy))
import GHC.TypeLits (KnownNat, natVal)

import Hedgehog (MonadGen, Property, (===), forAll, property, tripping)
import qualified Hedgehog.Gen as G
import qualified Hedgehog.Range as R
import Test.Tasty.HUnit (Assertion, (@?=))

import Crypto.Algorand.Signature (SecretKey, skFromBytes, toPublic)
import Data.Algorand.Address (Address, fromContractCode, fromPublicKey)
import Data.Algorand.MessagePack (Canonical (Canonical, unCanonical), EitherError)
import Data.Algorand.Transaction (StateSchema (..), Transaction (..), TransactionType (..))
import Data.Algorand.Transaction.Signed (SignedTransaction, getUnverifiedTransaction, signFromContractAccount, signSimple, verifyTransaction)

import Test.Crypto.Algorand.Signature (genPublicKey, genSecretKeyBytes)
import Test.Data.Algorand.Program (genProgram)
import Test.Data.Algorand.Transaction (genTransaction)
import Test.Data.Algorand.Transaction.Examples (example_21)


hprop_sign_verify_simple :: Property
hprop_sign_verify_simple = property $ do
  Just sk <- skFromBytes <$> forAll genSecretKeyBytes
  tx <- forAll genTransaction
  let tx' = tx { tSender = fromPublicKey (toPublic sk) }
  tripping tx' (signSimple sk) verifyTransaction

hprop_signSimple_sets_sender :: Property
hprop_signSimple_sets_sender = property $ do
  Just sk <- skFromBytes <$> forAll genSecretKeyBytes
  tx <- forAll genTransaction
  let tx' = getUnverifiedTransaction $ signSimple sk tx
  tSender tx' === fromPublicKey (toPublic sk)


hprop_sign_verify_contract :: Property
hprop_sign_verify_contract = property $ do
  program <- forAll genProgram
  tx <- forAll genTransaction
  let tx' = tx { tSender = fromContractCode program }
  tripping tx' (signFromContractAccount program []) verifyTransaction

hprop_signFromContractAccount_sets_sender :: Property
hprop_signFromContractAccount_sets_sender = property $ do
  program <- forAll genProgram
  tx <- forAll genTransaction
  let tx' = getUnverifiedTransaction $ signFromContractAccount program [] tx
  tSender tx' === fromContractCode program



data Signer
  = SignerSimple SecretKey
  | SignerContract ByteString
  deriving (Show)

signerSign :: Signer -> Transaction -> SignedTransaction
signerSign (SignerSimple sk) = signSimple sk
signerSign (SignerContract program) = signFromContractAccount program []

genSigner :: MonadGen m => m Signer
genSigner = G.choice
    [ SignerSimple <$> genSecretKey
    , SignerContract <$> genProgram
    ]
  where
    genSecretKey = G.justT (skFromBytes <$> genSecretKeyBytes)

hprop_json_encode_decode :: Property
hprop_json_encode_decode = property $ do
  signer <- forAll genSigner
  tx <- forAll genTransaction
  tripping (signerSign signer tx) toJSON fromJSON
