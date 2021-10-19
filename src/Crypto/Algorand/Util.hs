-- SPDX-FileCopyrightText: 2021 Serokell <https://serokell.io>
--
-- SPDX-License-Identifier: MPL-2.0

-- | Public key signatures used in Algorand.
module Crypto.Algorand.Util
  ( sign
  , verify
  ) where

import qualified Crypto.PubKey.Ed25519 as Sig

import Data.ByteArray (ByteArrayAccess)

import Crypto.Algorand.Key (PublicKey, SecretKey, getKeys)
import Crypto.Algorand.Signature.Simple (SimpleSignature (..))

-- | Produce a cryptographic signature for the data.
sign
  :: ByteArrayAccess dataBytes
  => SecretKey
  -- ^ Secret key used for signing.
  -> dataBytes
  -- ^ Bytes to sign.
  -> SimpleSignature
sign secret = SimpleSignature . Sig.sign sk pk
  where
    (sk, pk) = getKeys secret

-- | Verify a signature produced by 'sign'.
verify
  :: ByteArrayAccess dataBytes
  => PublicKey
  -- ^ Public key corresponding to the secret key used for singing.
  -> dataBytes
  -- ^ Originally signed bytes.
  -> SimpleSignature
  -- ^ Signature to verify.
  -> Bool
verify pk bs (SimpleSignature sig) = Sig.verify pk bs sig
