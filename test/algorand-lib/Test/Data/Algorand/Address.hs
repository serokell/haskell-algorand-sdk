-- SPDX-FileCopyrightText: 2021 Serokell <https://serokell.io>
--
-- SPDX-License-Identifier: MPL-2.0

{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Test.Data.Algorand.Address where

import Data.ByteString (ByteString)
import Data.ByteString.Base32 (decodeBase32, encodeBase32Unpadded)
import Data.Maybe (fromJust)

import Hedgehog (Property, forAll, property, tripping)
import Test.HUnit (Assertion)
import Test.Tasty.HUnit ((@?=))

import Crypto.Algorand.Signature (pkFromBytes)
import Data.Algorand.Address (Address, fromBytes, fromPublicKey, fromText, toPublicKey, toText)

import Test.Crypto.Algorand.Signature (genPublicKey)


-- Example from the documentation.
exampleAddress :: Address
exampleAddress = "VCMJKWOY5P5P7SKMZFFOCEROPJCZOTIJMNIYNUCKH7LRO45JMJP6UYBIJA"

exampleAddressBytes :: ByteString
exampleAddressBytes =
  "\168\152\149Y\216\235\250\255\201L\201J\225\DC2.zE\151M\tcQ\134\208J?\215\ETBs\169b_\234`(H"

examplePk :: ByteString
examplePk = "VCMJKWOY5P5P7SKMZFFOCEROPJCZOTIJMNIYNUCKH7LRO45JMJPQ===="


unit_decode_example_address :: Assertion
unit_decode_example_address = do
  exampleAddress @?= fromJust (fromBytes exampleAddressBytes)

unit_encode_example_address_bytes :: Assertion
unit_encode_example_address_bytes = do
  encodeBase32Unpadded exampleAddressBytes @?= toText exampleAddress

unit_example_pk_address :: Assertion
unit_example_pk_address = do
  let Right pkBytes = decodeBase32 examplePk
  let Just pk = pkFromBytes pkBytes
  fromPublicKey pk @?= exampleAddress

unit_example_address_pk :: Assertion
unit_example_address_pk = do
  let Right pkBytes = decodeBase32 examplePk
  let Just pk = pkFromBytes pkBytes
  let pk' = toPublicKey exampleAddress
  pk' @?= pk

-- | Encode-decode for addresses.
hprop_to_from_address :: Property
hprop_to_from_address = property $ do
    pk <- forAll $ genPublicKey
    tripping pk (toText . fromPublicKey) (fmap toPublicKey . fromText)
