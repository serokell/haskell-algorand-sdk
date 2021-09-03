-- SPDX-FileCopyrightText: 2021 Serokell <https://serokell.io>
--
-- SPDX-License-Identifier: MPL-2.0

{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Test.Data.Algorand.Transaction.Signed where

import Data.Aeson (fromJSON, toJSON)
import Data.ByteString (ByteString)
import Data.ByteString.Base64 (encodeBase64)
import qualified Data.ByteString.Lazy as BSL

import Hedgehog (MonadGen, Property, forAll, property, tripping, (===))
import qualified Hedgehog.Gen as G
import Test.Tasty.HUnit (Assertion, (@?=))

import Crypto.Algorand.Key (SecretKey, skFromBytes, toPublic)
import Data.Algorand.Address (fromContractCode, fromPublicKey)
import qualified Data.Algorand.MessagePack as MP
import Data.Algorand.Transaction (Transaction (..))
import Data.Algorand.Transaction.Signed (SignedTransaction, getUnverifiedTransaction,
                                         signFromContractAccount, signSimple, verifyTransaction)

import Test.Crypto.Algorand.Signature (genSecretKeyBytes)
import Test.Data.Algorand.Program (genProgram)
import Test.Data.Algorand.Transaction (genTransaction)
import Test.Data.Algorand.Transaction.Examples (example_21)


hprop_sign_verify_simple :: Property
hprop_sign_verify_simple = property $ do
  Just sk <- skFromBytes <$> forAll genSecretKeyBytes
  tx <- forAll genTransaction
  let expectedTx = tx { tSender = fromPublicKey (toPublic sk) }
  (verifyTransaction $ signSimple sk tx) === Just expectedTx

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



unit_sign_as_contract :: Assertion
unit_sign_as_contract = do
    -- Generated using Python SDK.
    let program = "\x01\x20\x01\x01\x22"
    let signed = signFromContractAccount program [] example_21
    encodeBase64 (BSL.toStrict . MP.pack . MP.Canonical $ signed) @?= expected
  where
    expected = "gqRsc2lngaFsxAUBIAEBIqN0eG6LpGFwYWGRxAR0ZXN0pGFwYW6kY2FsbKRhcGF0ksQgACoyATtqMON+4ohUJO59fQVV6uCTn7aa/GvfndL+5/7EIAAHBAuPYqMysOAF8ALIwKUWNGgBCjFYJ8bPUnx4aXnnpGFwZmGSzRWzzRoKpGFwaWRko2ZlZc0E0qJmds0jKKJnaMQgMf0h6zjkEIEZPtNM3zsrg+iHQFS0fZxhgr7w35I464OibHbNIzKjc25kxCD2di2sdbGZfWwslhgGgFB0kNeVES/+f7dgsnOK+cfxraR0eXBlpGFwcGw="
