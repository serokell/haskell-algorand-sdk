-- SPDX-FileCopyrightText: 2021 Serokell <https://serokell.io>
--
-- SPDX-License-Identifier: MPL-2.0

-- | Logic signature used in Algorand.
module Crypto.Algorand.Signature.Logic
  ( LogicSignature (..)
  ) where

import Data.Aeson.TH (deriveJSON)
import Data.ByteString (ByteString)
import Data.String (IsString)
import GHC.Generics (Generic)

import Data.Algorand.MessagePack (MessagePackObject (..), MessageUnpackObject (..),
                                  NonZeroValue (..), (&), (.:?), (.=))
import Network.Algorand.Api.Json (algorandTrainOptions)

data LogicSignature = LogicSignature
  -- TODO: Only contract account signature is supported.
  { lsArgs :: [ByteString]
  , lsLogic :: ByteString
  } deriving (Eq, Generic, Show)
$(deriveJSON algorandTrainOptions 'LogicSignature)

instance NonZeroValue LogicSignature where
  isNonZero _ = True

logicSignatureFieldName :: IsString s => String -> s
logicSignatureFieldName = \case
  "lsLogic" -> "l"
  "lsArgs" -> "arg"
  x -> error $ "Unmapped logic signature field name: " <> x

instance MessagePackObject LogicSignature where
  toCanonicalObject = \case
    LogicSignature{..} -> mempty
      & f "lsLogic" .= lsLogic
      & f "lsArgs" .= lsArgs
    where
      f = logicSignatureFieldName

instance MessageUnpackObject LogicSignature where
  fromCanonicalObject o = do
    lsLogic <- o .:? f "lsLogic"
    lsArgs <- o .:? f "lsArgs"
    pure LogicSignature{..}
    where
      f = logicSignatureFieldName
