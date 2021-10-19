-- SPDX-FileCopyrightText: 2021 Serokell <https://serokell.io>
--
-- SPDX-License-Identifier: MPL-2.0

-- | Tools for manipulating Algorand account addresses.
module Data.Algorand.Address
  ( Address
  , zero

  , fromBytes
  , toText
  , fromText

  , fromPublicKey
  , toPublicKey

  , fromContractCode
  ) where

import Prelude hiding (length)

import qualified Data.ByteArray.Sized (zero)
import qualified Data.Text as T

import Control.Applicative (empty)
import Control.Monad (guard)
import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.ByteArray (ByteArrayAccess, Bytes, View, convert, eq, length, view)
import Data.ByteArray.Sized (SizedByteArray, sizedByteArray, unSizedByteArray)
import Data.ByteString (ByteString)
import Data.ByteString.Base32 (decodeBase32Unpadded, encodeBase32Unpadded)
import Data.String (IsString (fromString))
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Fmt (Buildable (..))
import Servant.API (ToHttpApiData (toQueryParam))

import qualified Data.Algorand.MessagePack (zero)

import Crypto.Algorand.Hash (hash32)
import Crypto.Algorand.Key (PublicKey, pkFromBytes, pkSize)
import Data.Algorand.MessagePack (AlgoMessagePack (..), CanonicalZero, NonZeroValue (isNonZero))

-- | The length of the checksum appended to a public key.
checksumSize :: Int
checksumSize = 4

-- | An address on the Algorand blockchain.
--
-- Internally, this is just 32 bytes, and when seialised it is sent
-- over the network as raw bytes, however when obtaining it from external
-- sources, it contains an additional checksum and is encoded in base32.
newtype Address = Address (SizedByteArray 32 Bytes)
  deriving (ByteArrayAccess, Eq)

instance Show Address where
  show = T.unpack . toText

instance Buildable Address where
  build = build . toText

instance IsString Address where
  fromString s = case fromText (T.pack s) of
    Nothing -> error "Invalid address"
    Just a -> a

instance NonZeroValue Address where
  isNonZero = (/= zero)

instance CanonicalZero Address where
  zero = Data.Algorand.Address.zero

instance AlgoMessagePack Address where
  toAlgoObject (Address bs) = toAlgoObject @Bytes (convert bs)
  fromAlgoObject o = fromAlgoObject @Bytes o >>= \bs ->
    case sizedByteArray bs of
      Nothing -> fail "Invalid address bytes length"
      Just sized -> pure $ Address sized

instance ToHttpApiData Address where
  toQueryParam = toText

instance ToJSON Address where
  toEncoding = toEncoding . toText
  toJSON = toJSON . toText

instance FromJSON Address where
  parseJSON o = parseJSON o >>= \t -> maybe empty pure (fromText t)

-- | Dummy zero address. Can be used as a placeholder value.
zero :: Address
zero = Address Data.ByteArray.Sized.zero

-- | Try to interpret raw bytes as 'Address'.
--
-- The input is a bytestring containing a public key and a checksum.
-- This function will return @Nothing@ if the length is wrong or the checksum does not match.
fromBytes :: ByteString -> Maybe Address
fromBytes bytes = do
  guard (length bytes == pkSize + checksumSize)
  sized <- sizedByteArray $ convert $ view bytes 0 pkSize
  guard (checksum sized `eq` view bytes pkSize 4)
  pure $ Address sized

-- | Display an 'Address' using the Algorand human-readable representation.
toText :: Address -> Text
toText (Address bs) = encodeBase32Unpadded (convert bs <> convert (checksum bs))

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
fromPublicKey pk = case sizedByteArray (convert pk) of
  Just sized -> Address sized
  Nothing -> error "Impossible: the public key must be 32 bytes long"

-- | Convert an Algorand 'Address' to the corresponding 'PublicKey'.
--
-- This function checks the integrity by verifying the checksum embedded
-- in Algorand addresses.
toPublicKey :: Address -> Maybe PublicKey
toPublicKey (Address bs) = pkFromBytes bs

-- | Get an address of a stateless smart contract.
--
-- This is used in conjunction with Logic Signatures as a Cotnract Account.
fromContractCode
  :: ByteString
  -- ^ Compiled contract code.
  -> Address
fromContractCode = Address . hash32 . ("Program" <>)

-- | Compute the checksum of a public key.
checksum :: ByteArrayAccess bs => bs -> View Bytes
checksum bs = view (unSizedByteArray $ hash32 bs) (32 - checksumSize) checksumSize
