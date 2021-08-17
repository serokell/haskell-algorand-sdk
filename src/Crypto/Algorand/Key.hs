-- SPDX-FileCopyrightText: 2021 Serokell <https://serokell.io>
--
-- SPDX-License-Identifier: MPL-2.0

-- | Public key signatures used in Algorand.
module Crypto.Algorand.Key
  ( SecretKey
  , PublicKey
  , getKeys
  , keypair
  , toPublic

  , skToText
  , skFromText

  , pkSize
  , pkFromBytes

  , skSize
  , skFromBytes
  ) where

import qualified Crypto.PubKey.Ed25519 as Sig
import qualified Data.ByteString as BS

import Control.Monad (guard)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Crypto.Error (CryptoFailable (CryptoFailed, CryptoPassed))
import Data.ByteArray (ByteArrayAccess, convert)
import Data.ByteString (ByteString)
import Data.ByteString.Base64 (decodeBase64, encodeBase64)
import Data.Text (Text)

import Network.Algorand.Api.Json ()

-- | Signing secret key.
data SecretKey where
  -- We include the public key too, because ed25519 needs it
  -- for signing and because Algorand’s “base64 private key”
  -- is essentially a pair of secret and public keys anyway.
  SecretKey :: Sig.SecretKey -> Sig.PublicKey -> SecretKey

getKeys :: SecretKey -> (Sig.SecretKey, Sig.PublicKey)
getKeys (SecretKey sk pk) = (sk, pk)

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

-- | Export a secret key in base64.
--
-- The output of this function contains raw unprotected key material!
skToText :: SecretKey -> Text
skToText (SecretKey sk pk) = encodeBase64 (convert sk <> convert pk)

-- | Import a secret key in base64.
--
-- This is the opposite of 'skToText'.
--
-- The encoding used by Algorand is base64 of the concatenation of
-- sk and pk bytes, so this function will fail if the pk and sk
-- do not match.
skFromText :: ByteString -> Maybe SecretKey
skFromText t = do
  bs <- case decodeBase64 t of
    Left _ -> Nothing
    Right r -> Just r
  let (skBytes, pkBytes) = BS.splitAt skSize bs
  sk <- skFromBytes skBytes
  pk <- pkFromBytes pkBytes
  guard $ pk == toPublic sk
  pure sk

-- | Size of a 'PublicKey' in bytes.
pkSize :: Int
pkSize = Sig.publicKeySize

-- | Try to interpret bytes as a 'PublicKey'.
pkFromBytes
  :: ByteArrayAccess pkBytes
  => pkBytes
  -- ^ Bytes containing the key.
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
  => skBytes
  -- ^ Bytes containing the key.
  -> Maybe SecretKey
skFromBytes bs = case Sig.secretKey bs of
  CryptoPassed sk -> Just $ SecretKey sk (Sig.toPublic sk)
  CryptoFailed _ -> Nothing
