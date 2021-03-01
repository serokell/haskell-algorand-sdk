-- SPDX-FileCopyrightText: 2021 Serokell <https://serokell.io>
--
-- SPDX-License-Identifier: MPL-2.0

-- | Public key signatures used in Algorand.
module Crypto.Algorand.Signature
  ( SecretKey
  , PublicKey
  , keypair
  , toPublic

  , pkSize
  , pkFromBytes

  , skSize
  , skFromBytes

  , Signature
  , sign
  , verify
  ) where

import Data.ByteArray (ByteArrayAccess, Bytes, convert)
import Data.MessagePack (MessagePack (fromObject, toObject))
import Control.Monad.IO.Class (MonadIO, liftIO)
import Crypto.Error (CryptoFailable (CryptoFailed, CryptoPassed))
import qualified Crypto.PubKey.Ed25519 as Sig

import Data.Algorand.MessagePack (NonZeroValue (isNonZero))


-- | Signing secret key.
data SecretKey where
  -- We include the public key too, because ed25519 needs it
  -- for signing and because Algorand’s “base64 private key”
  -- is essentially a pair of secret and public keys anyway.
  SecretKey :: Sig.SecretKey -> Sig.PublicKey -> SecretKey

-- | Signing public key.
type PublicKey = Sig.PublicKey

-- | Generate a new signing keypair.
keypair :: MonadIO m => m SecretKey
keypair = do
  sk <- liftIO Sig.generateSecretKey
  pure $ SecretKey sk (Sig.toPublic sk)

-- | Compute the public key corresponding to the given secret key.
toPublic :: SecretKey -> PublicKey
toPublic (SecretKey _ pk) = pk


-- | Size of a 'PublicKey' in bytes.
pkSize :: Int
pkSize = Sig.publicKeySize

-- | Try to interpret bytes as a 'PublicKey'.
pkFromBytes
  :: ByteArrayAccess pkBytes
  => pkBytes  -- ^ Bytes containing the key.
  -> Maybe PublicKey
pkFromBytes bs = case Sig.publicKey bs of
  CryptoPassed pk -> Just pk
  CryptoFailed _ -> Nothing

-- | Size of a 'SecretKey' in bytes.
skSize :: Int
skSize = Sig.secretKeySize

-- | Try to interpret bytes as a 'SecretKey'.
skFromBytes
  :: ByteArrayAccess skBytes
  => skBytes  -- ^ Bytes containing the key.
  -> Maybe SecretKey
skFromBytes bs = case Sig.secretKey bs of
  CryptoPassed sk -> Just $ SecretKey sk (Sig.toPublic sk)
  CryptoFailed _ -> Nothing


-- | Cryptographic signature.
newtype Signature = Signature Sig.Signature
  deriving (ByteArrayAccess, Show)

instance NonZeroValue Signature where
  isNonZero _ = True

instance MessagePack Signature where
  toObject (Signature sig) = toObject @Bytes . convert $ sig
  fromObject o = do
    bs <- fromObject @Bytes o
    case sigFromBytes bs of
      Nothing -> fail "Malformed signature bytes"
      Just sig -> pure sig

sigFromBytes
  :: ByteArrayAccess sigBytes
  => sigBytes  -- ^ Bytes containing the signature.
  -> Maybe Signature
sigFromBytes bs = case Sig.signature bs of
  CryptoPassed sig -> Just $ Signature sig
  CryptoFailed _ -> Nothing

-- | Produce a cryptograhic signature for the data.
sign
  :: ByteArrayAccess dataBytes
  => SecretKey  -- ^ Secret key used for signing.
  -> dataBytes  -- ^ Bytes to sign.
  -> Signature
sign (SecretKey sk pk) = Signature . Sig.sign sk pk

-- | Verify a signature produced by 'sign'.
verify
  :: ByteArrayAccess dataBytes
  => PublicKey  -- ^ Public key corresponding to the secret key used for singing.
  -> dataBytes  -- ^ Originally signed bytes.
  -> Signature  -- ^ Signature to verify.
  -> Bool
verify pk bs (Signature sig) = Sig.verify pk bs sig
