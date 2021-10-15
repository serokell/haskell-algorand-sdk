-- SPDX-FileCopyrightText: 2021 Serokell <https://serokell.io>
--
-- SPDX-License-Identifier: MPL-2.0

-- | Public key signatures used in Algorand.
module Crypto.Algorand.Signature
  ( SignatureType (..)
  , SimpleSignature (..)
  , LogicSignature (..)
  , MultiSignature (..)
  ) where

import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.String (IsString)
import Data.Text (Text)
import GHC.Generics (Generic)

import Crypto.Algorand.Signature.Logic (LogicSignature (..))
import Crypto.Algorand.Signature.Multi (MultiSignature (..))
import Crypto.Algorand.Signature.Simple (SimpleSignature (..))
import Data.Algorand.MessagePack (MessagePackObject (..), MessageUnpackObject (..),
                                  NonZeroValue (..), (&), (.:>?), (.:??), (.=), (.=<))
import Data.Algorand.MessagePack.Json (parseCanonicalJson, toCanonicalJson)
import Network.Algorand.Api.Json ()

-- | Types of signatures.
data SignatureType
  = SignatureSimple SimpleSignature
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
  x -> error $ "Unmapped signature constructor: " <> x

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
