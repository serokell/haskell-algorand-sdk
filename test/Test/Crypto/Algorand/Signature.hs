-- SPDX-FileCopyrightText: 2021 Serokell <https://serokell.io>
--
-- SPDX-License-Identifier: MPL-2.0

{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Test.Crypto.Algorand.Signature where

import Data.Maybe (fromJust)
import Data.Text.Encoding (encodeUtf8)
import Hedgehog (Property, forAll, property, tripping)
import Test.Tasty.HUnit (Assertion, (@?=))

import Crypto.Algorand.Key (skFromBytes, skFromText, skToText, toPublic)
import Data.Algorand.Address (fromPublicKey)

import Test.Domain (sender)
import Test.Gen (genSecretKeyBytes)

unit_example_sk_base64 :: Assertion
unit_example_sk_base64 = do
    (fromPublicKey . toPublic $ exampleSk) @?= sender
  where
    exampleSk = fromJust $ skFromText
      "hdhQ/fKNOVHg8D5kLzE21SKHKLyt7DSMAlYq4IUnepIJ+9J2LAj4bFrmv23Xp6kB3mZ111Dgfoxcdphkfbbh/Q=="

hprop_secret_base64_encode_decode :: Property
hprop_secret_base64_encode_decode = property $ do
  Just sk <- skFromBytes <$> forAll genSecretKeyBytes
  tripping sk skToText (skFromText . encodeUtf8)
