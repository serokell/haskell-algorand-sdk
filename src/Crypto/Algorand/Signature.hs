-- SPDX-FileCopyrightText: 2021 Serokell <https://serokell.io>
--
-- SPDX-License-Identifier: MPL-2.0

-- | Public key signatures used in Algorand.
module Crypto.Algorand.Signature
  ( SignatureType (..)
  , Signature (..)
  , LogicSignature (..)
  , MultiSignature (..)
  ) where

import qualified Crypto.PubKey.Ed25519 as Sig

import Crypto.Error (CryptoFailable (CryptoFailed, CryptoPassed))
import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.Aeson.Types (parseFail)
import Data.ByteArray (ByteArrayAccess, Bytes, convert)
import Data.ByteString (ByteString)
import Data.String (IsString)
import Data.Text (Text)
import GHC.Generics (Generic)

import Data.Algorand.MessagePack (AlgoMessagePack (..), MessagePackObject (..),
                                  MessageUnpackObject (..), NonZeroValue (..), (&), (.:>?), (.:?),
                                  (.:??), (.=), (.=<))
import Data.Algorand.MessagePack.Json (parseCanonicalJson, toCanonicalJson)
import Network.Algorand.Api.Json ()

-- | Types of transaction signatures.
data SignatureType
  = SignatureSimple Signature
  | SignatureMulti MultiSignature
  | SignatureLogic LogicSignature
  deriving (Eq, Generic, Show)

instance NonZeroValue SignatureType where
  isNonZero _ = True

signatureType :: IsString s => String -> s
signatureType = \case
  "SignatureSimple" -> "sig"
  "SignatureMulti" -> "msig"
  "SignatureLogic" -> "lsig"
  x -> error $ "Unmapped transaction signature constructor: " <> x

instance MessagePackObject SignatureType where
  toCanonicalObject = \case
    SignatureSimple sig -> mempty
      & t "SignatureSimple" .= sig
    SignatureMulti msig -> mempty
      & t "SignatureMulti" .=< msig
    SignatureLogic lsig -> mempty
      & t "SignatureLogic" .=< lsig
    where
      t = signatureType :: String -> Text

instance MessageUnpackObject SignatureType where
  fromCanonicalObject o = o .:?? t "SignatureSimple" >>= \case
    Just sig -> pure $ SignatureSimple sig
    Nothing -> o .:>? t "SignatureMulti" >>= \case
      Just msig -> pure $ SignatureMulti msig
      Nothing -> o .:>? t "SignatureLogic" >>= \case
        Just lsig -> pure $ SignatureLogic lsig
        Nothing -> fail "Unsupported or missing signature"
    where
      t = signatureType :: String -> Text

instance ToJSON SignatureType where
  toJSON = toCanonicalJson

instance FromJSON SignatureType where
  parseJSON = parseCanonicalJson

data MultiSignature = MultiSignature
  deriving (Eq, Generic, Show)

instance NonZeroValue MultiSignature where
  isNonZero _ = True


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
  => sigBytes
  -- ^ Bytes containing the signature.
  -> Maybe Signature
sigFromBytes bs = case Sig.signature bs of
  CryptoPassed sig -> Just $ Signature sig
  CryptoFailed _ -> Nothing


instance MessagePackObject MultiSignature where
  toCanonicalObject MultiSignature = mempty  -- TODO

instance MessageUnpackObject MultiSignature where
  fromCanonicalObject _ = pure MultiSignature  -- TODO

instance ToJSON MultiSignature where
  toJSON = toCanonicalJson

instance FromJSON MultiSignature where
  parseJSON = parseCanonicalJson

data LogicSignature = ContractAccountSignature
  -- TODO: Only contract account signature is supported.
  { lsLogic :: ByteString
  , lsArgs :: [ByteString]
  }
  deriving (Eq, Generic, Show)


instance NonZeroValue LogicSignature where
  isNonZero _ = True

logicSignatureFieldName :: IsString s => String -> s
logicSignatureFieldName = \case
  "lsLogic" -> "l"
  "lsArgs" -> "arg"
  x -> error $ "Unmapped logic signature field name: " <> x

instance MessagePackObject LogicSignature where
  toCanonicalObject = \case
    ContractAccountSignature{..} -> mempty
      & f "lsLogic" .= lsLogic
      & f "lsArgs" .= lsArgs
    where
      f = logicSignatureFieldName

instance MessageUnpackObject LogicSignature where
  fromCanonicalObject o = do
    lsLogic <- o .:? f "lsLogic"
    lsArgs <- o .:? f "lsArgs"
    pure ContractAccountSignature{..}
    where
      f = logicSignatureFieldName

instance ToJSON LogicSignature where
  toJSON = toCanonicalJson

instance FromJSON LogicSignature where
  parseJSON = parseCanonicalJson
