-- SPDX-FileCopyrightText: 2021 Serokell <https://serokell.io>
--
-- SPDX-License-Identifier: MPL-2.0

-- | Multi signature used in Algorand.
module Crypto.Algorand.Signature.Multi
  ( MultiSignature (..)
  ) where

import Data.Aeson (FromJSON (..), ToJSON (..))
import GHC.Generics (Generic)

import Data.Algorand.MessagePack (MessagePackObject (..), MessageUnpackObject (..),
                                  NonZeroValue (..))
import Data.Algorand.MessagePack.Json (parseCanonicalJson, toCanonicalJson)
import Network.Algorand.Api.Json ()

data MultiSignature = MultiSignature
  deriving (Eq, Generic, Show)

instance NonZeroValue MultiSignature where
  isNonZero _ = True

instance MessagePackObject MultiSignature where
  toCanonicalObject MultiSignature = mempty  -- TODO

instance MessageUnpackObject MultiSignature where
  fromCanonicalObject _ = pure MultiSignature  -- TODO

instance ToJSON MultiSignature where
  toJSON = toCanonicalJson

instance FromJSON MultiSignature where
  parseJSON = parseCanonicalJson
