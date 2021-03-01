-- SPDX-FileCopyrightText: 2021 Serokell <https://serokell.io>
--
-- SPDX-License-Identifier: MPL-2.0

-- | Tools for manipulating Algorand account addresses.
module Data.Algorand.Address
  ( Address

  , fromBytes
  , toText
  , fromText

  , fromPublicKey
  , toPublicKey
  ) where

import Prelude hiding (length)

import Control.Monad (guard)
import Data.ByteArray (Bytes, ByteArrayAccess, View, convert, eq, length, view)
import Data.ByteArray.Sized (unSizedByteArray)
import Data.ByteString (ByteString)
import Data.ByteString.Base32 (decodeBase32Unpadded, encodeBase32Unpadded)
import Data.String (IsString (fromString))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)

import Crypto.Algorand.Hash (hash32)
import Crypto.Algorand.Signature (PublicKey, pkFromBytes, pkSize)
import Data.Algorand.MessagePack (NonZeroValue (isNonZero), MessagePack (fromObject, toObject))


-- | The length of the checksum appended to a public key.
checksumSize :: Int
checksumSize = 4


-- | An address on the Algorand blockchain.
--
-- Internally, this is just a public key, and when seialised it is sent
-- over the network as a public key, however when obtaining it from external
-- sources, it contains an additional checksum and is encoded in base32.
newtype Address = Address PublicKey
  deriving (Eq)

instance Show Address where
  show = T.unpack . toText

instance IsString Address where
  fromString s = case fromText (T.pack s) of
    Nothing -> error "Invalid address"
    Just a -> a

instance NonZeroValue Address where
  isNonZero _ = True

instance MessagePack Address where
  toObject (Address pk) = toObject @Bytes (convert pk)
  fromObject o = fromObject @Bytes o >>= \bs ->
    case pkFromBytes bs of
      Nothing -> fail "Invalid address (public key)"
      Just pk -> pure $ Address pk

-- | Try to interpret raw bytes as 'Address'.
--
-- The input is a bytestring containing a public key and a checksum.
-- This function will return @Nothing@ if the length is wrong or the checksum does not match.
fromBytes :: ByteString -> Maybe Address
fromBytes bytes = do
  guard (length bytes == pkSize + checksumSize)
  pk <- pkFromBytes $ view bytes 0 pkSize
  guard (checksum pk `eq` view bytes pkSize 4)
  pure $ Address pk


-- | Display an 'Address' using the Algorand human-readable representation.
toText :: Address -> Text
toText (Address pk) = encodeBase32Unpadded (convert pk <> convert (checksum pk))

-- | Decode an 'Address' from a human-readable representation.
--
-- This is the inverse of 'toText'. The input is a base32-encoded
-- bytestring containing a public key and a checksum.
-- This function will return @Nothing@ if the length is wrong or the checksum does not match.
fromText :: Text -> Maybe Address
fromText t = case decodeBase32Unpadded (encodeUtf8 t) of
  Left _ -> Nothing
  Right bs -> fromBytes bs


-- | Convert a 'PublicKey' to the corresponding algorand 'Address'.
fromPublicKey :: PublicKey -> Address
fromPublicKey = Address

-- | Convert an Algorand 'Address' to the corresponding 'PublicKey'.
--
-- This function checks the integrity by verifying the checksum embedded
-- in Algorand addresses.
toPublicKey :: Address -> PublicKey
toPublicKey (Address pk) = pk


-- | Compute the checksum of a public key.
checksum :: ByteArrayAccess bs => bs -> View Bytes
checksum pk = view (unSizedByteArray $ hash32 pk) (32 - checksumSize) checksumSize
