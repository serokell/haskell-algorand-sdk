-- SPDX-FileCopyrightText: 2021 Serokell <https://serokell.io>
--
-- SPDX-License-Identifier: MPL-2.0

-- | Public key signatures used in Algorand.
module Crypto.Algorand.Signature
  ( SecretKey
  , PublicKey
  , keypair
  , toPublic

  , skToText
  , skFromText

  , pkSize
  , pkFromBytes

  , skSize
  , skFromBytes

  , Signature
  , sign
  , verify
  ) where

import Control.Monad (guard)
import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.Aeson.Types (parseFail)
import Data.ByteArray (ByteArrayAccess, Bytes, convert)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.ByteString.Base64 (decodeBase64, encodeBase64)
import Data.Text (Text)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Crypto.Error (CryptoFailable (CryptoFailed, CryptoPassed))
import qualified Crypto.PubKey.Ed25519 as Sig

import Data.Algorand.MessagePack (AlgoMessagePack (..), NonZeroValue (isNonZero))
import Network.Algorand.Node.Api.Json ()  -- instances for Bytes


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
  deriving (ByteArrayAccess, Eq, Show)

instance NonZeroValue Signature where
  isNonZero _ = True

instance AlgoMessagePack Signature where
  toAlgoObject (Signature sig) = toAlgoObject @Bytes . convert $ sig
  fromAlgoObject o = do
    bs <- fromAlgoObject @Bytes o
    case sigFromBytes bs of
      Nothing -> fail "Malformed signature bytes"
      Just sig -> pure sig

instance ToJSON Signature where
  toJSON = toJSON @Bytes . convert
  toEncoding = toEncoding @Bytes . convert

instance FromJSON Signature where
  parseJSON o = do
    bs <- parseJSON @Bytes o
    case sigFromBytes bs of
      Nothing -> parseFail "Malformed signature"
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
