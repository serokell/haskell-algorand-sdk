-- SPDX-FileCopyrightText: 2021 Serokell <https://serokell.io>
--
-- SPDX-License-Identifier: MPL-2.0

module Test.Crypto.Algorand.Signature where

import Data.ByteString (ByteString)

import Hedgehog (MonadGen)
import qualified Hedgehog.Gen as G
import qualified Hedgehog.Range as R

import Crypto.Algorand.Signature (PublicKey, pkFromBytes, pkSize, skFromBytes, skSize)


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
