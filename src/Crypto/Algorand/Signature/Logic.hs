-- SPDX-FileCopyrightText: 2021 Serokell <https://serokell.io>
--
-- SPDX-License-Identifier: MPL-2.0

-- | Logic signature used in Algorand.
module Crypto.Algorand.Signature.Logic
  ( LogicSignature (..)
  ) where

import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.ByteString (ByteString)
import Data.String (IsString)
import GHC.Generics (Generic)

import Data.Algorand.MessagePack (MessagePackObject (..), MessageUnpackObject (..),
                                  NonZeroValue (..), (&), (.:?), (.=))
import Data.Algorand.MessagePack.Json (parseCanonicalJson, toCanonicalJson)
import Network.Algorand.Api.Json ()

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
