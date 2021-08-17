-- SPDX-FileCopyrightText: 2021 Serokell <https://serokell.io>
--
-- SPDX-License-Identifier: MPL-2.0
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns -Wno-orphans #-}

module Test.Crypto.Algorand.Signature where

import Data.ByteString (ByteString)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)

import Hedgehog (MonadGen, Property, forAll, property, tripping)
import qualified Hedgehog.Gen as G
import qualified Hedgehog.Range as R
import Test.Tasty.HUnit (Assertion, (@?=))

import Crypto.Algorand.Key (PublicKey, SecretKey, pkFromBytes, pkSize, skFromBytes, skFromText,
                            skSize, skToText, toPublic)
import Data.Algorand.Address (fromPublicKey)


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


unit_example_sk_base64 :: Assertion
unit_example_sk_base64 = do
    (fromPublicKey . toPublic $ exampleSk) @?= exampleAddress
  where
    Just exampleSk = skFromText "hdhQ/fKNOVHg8D5kLzE21SKHKLyt7DSMAlYq4IUnepIJ+9J2LAj4bFrmv23Xp6kB3mZ111Dgfoxcdphkfbbh/Q=="
    exampleAddress = "BH55E5RMBD4GYWXGX5W5PJ5JAHPGM5OXKDQH5DC4O2MGI7NW4H6VOE4CP4"


hprop_secret_base64_encode_decode :: Property
hprop_secret_base64_encode_decode = property $ do
  Just sk <- skFromBytes <$> forAll genSecretKeyBytes
  tripping sk skToText (skFromText . encodeUtf8)

-- | This is here only to make @tripping@ work.
instance Show SecretKey where
  show = T.unpack . skToText
-- | This is here only to make @tripping@ work.
instance Eq SecretKey where
  sk1 == sk2 = skToText sk1 == skToText sk2
