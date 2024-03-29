-- SPDX-FileCopyrightText: 2021 Serokell <https://serokell.io>
--
-- SPDX-License-Identifier: MPL-2.0

{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Test.Data.Algorand.Transaction.Signed
  ( unit_sign_as_contract

  , hprop_sign_verify_simple
  , hprop_signSimple_sets_sender
  , hprop_sign_verify_contract
  , hprop_signFromContractAccount_sets_sender
  , hprop_json_encode_decode
  ) where

import qualified Data.ByteString.Lazy as BSL

import Data.Aeson (fromJSON, toJSON)
import Data.ByteString.Base64 (encodeBase64)
import Hedgehog (Property, forAll, property, tripping, (===))
import Test.Tasty.HUnit (Assertion, (@?=))

import qualified Data.Algorand.MessagePack as MP

import Crypto.Algorand.Key (skFromBytes, toPublic)
import Data.Algorand.Address (fromContractCode, fromPublicKey)
import Data.Algorand.Transaction (Transaction (..))
import Data.Algorand.Transaction.Signed (getUnverifiedTransaction, signFromContractAccount,
                                         signSimple, verifyTransaction)

import Test.Data.Algorand.Transaction.Examples (example_21)
import Test.Domain (signerSign)
import Test.Gen (genSecretKeyBytes, genSigner, genTransaction, genProgram)

unit_sign_as_contract :: Assertion
unit_sign_as_contract = do
    -- Generated using Python SDK.
    let program = "\x01\x20\x01\x01\x22"
    let signed = signFromContractAccount program [] example_21
    encodeBase64 (BSL.toStrict . MP.pack . MP.Canonical $ signed) @?= expected
  where
    expected = "gqRsc2lngaFsxAUBIAEBIqN0eG6KpGFwYWGRxAR0ZXN0pGFwYXSSxCAAKjIBO2ow437iiFQk7n19BVXq4JOftpr8a9+d0v7n/sQgAAcEC49iozKw4AXwAsjApRY0aAEKMVgnxs9SfHhpeeekYXBmYZLNFbPNGgqkYXBpZGSjZmVlzQTSomZ2zSMoomdoxCAx/SHrOOQQgRk+00zfOyuD6IdAVLR9nGGCvvDfkjjrg6Jsds0jMqNzbmTEIPZ2Lax1sZl9bCyWGAaAUHSQ15URL/5/t2Cyc4r5x/GtpHR5cGWkYXBwbA=="

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

hprop_json_encode_decode :: Property
hprop_json_encode_decode = property $ do
  signer <- forAll genSigner
  tx <- forAll genTransaction
  tripping (signerSign signer tx) toJSON fromJSON
