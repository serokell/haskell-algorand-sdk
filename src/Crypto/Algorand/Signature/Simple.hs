-- SPDX-FileCopyrightText: 2021 Serokell <https://serokell.io>
--
-- SPDX-License-Identifier: MPL-2.0

-- | Simple signature used in Algorand.
module Crypto.Algorand.Signature.Simple
  ( SimpleSignature (..)
  ) where

import qualified Crypto.PubKey.Ed25519 as Sig

import Crypto.Error (CryptoFailable (CryptoFailed, CryptoPassed))
import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.Aeson.Types (parseFail)
import Data.ByteArray (ByteArrayAccess, Bytes, convert)

import Data.Algorand.MessagePack (AlgoMessagePack (..), NonZeroValue (..))
import Network.Algorand.Api.Json ()

-- | Cryptographic signature.
newtype SimpleSignature = SimpleSignature Sig.Signature
  deriving (ByteArrayAccess, Eq, Show)

instance NonZeroValue SimpleSignature where
  isNonZero _ = True

instance AlgoMessagePack SimpleSignature where
  toAlgoObject (SimpleSignature sig) = toAlgoObject @Bytes . convert $ sig
  fromAlgoObject o = do
    bs <- fromAlgoObject @Bytes o
    case sigFromBytes bs of
      Nothing -> fail "Malformed signature bytes"
      Just sig -> pure sig

instance ToJSON SimpleSignature where
  toJSON = toJSON @Bytes . convert
  toEncoding = toEncoding @Bytes . convert

instance FromJSON SimpleSignature where
  parseJSON o = do
    bs <- parseJSON @Bytes o
    case sigFromBytes bs of
      Nothing -> parseFail "Malformed signature"
      Just sig -> pure sig

-- | Create signature from bytes.
sigFromBytes
  :: ByteArrayAccess sigBytes
  => sigBytes
  -- ^ Bytes containing the signature.
  -> Maybe SimpleSignature
sigFromBytes bs = case Sig.signature bs of
  CryptoPassed sig -> Just $ SimpleSignature sig
  CryptoFailed _ -> Nothing
