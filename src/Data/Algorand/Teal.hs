-- SPDX-FileCopyrightText: 2021 Serokell <https://serokell.io>
--
-- SPDX-License-Identifier: MPL-2.0

-- | Types and helpers related to Transaction Execution Approval Language (TEAL).
module Data.Algorand.Teal
  ( TealCode (..)
  , TealCompilationResult (..)
  , TealKeyValueStore (..)
  , TealValue (..)

  , tealValueBytesType
  , tealValueUintType
  ) where

import Control.Monad (liftM2)
import Data.Aeson (FromJSON, ToJSON, object, withObject, (.:), (.=))
import Data.Aeson.TH (deriveJSON)
import qualified Data.Aeson.Types as A
import Data.ByteString (ByteString)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Word (Word64)

import Data.Algorand.Address (Address)
import Network.Algorand.Api.Json (algorandTrainOptions)

tealValueBytesType :: Word64
tealValueBytesType = 1

tealValueUintType :: Word64
tealValueUintType = 2

-- | Represents a TEAL value.
data TealValue = TealValue
  { tvBytes :: ByteString
  -- ^ bytes value.
  , tvUint :: Word64
  -- ^ uint type.
  , tvType :: Word64
  -- ^ value type.
  } deriving stock (Show, Eq)
$(deriveJSON algorandTrainOptions 'TealValue)

-- | Represents a key-value store for use in an application.
newtype TealKeyValueStore = TealKeyValueStore
  { unTealKeyValueStore :: Map ByteString TealValue
  } deriving newtype (Show, Eq)

instance ToJSON TealKeyValueStore where
  toJSON (TealKeyValueStore entries) =
    A.listValue
      (\(key, value) -> object ["key" .= key, "value" .= value])
      (Map.toList entries)

instance FromJSON TealKeyValueStore where
  parseJSON v =
    fmap (TealKeyValueStore . Map.fromList) $
      A.listParser
        ( withObject "key/value pair" $ \o ->
            liftM2 (,) (o .: "key") (o .: "value")
        ) v

newtype TealCode = TealCode
  { unTealCode :: ByteString
  } deriving newtype (Show, Eq, FromJSON, ToJSON)

data TealCompilationResult = TealCompilationResult
  { tcrHash :: Address
  , tcrResult :: TealCode
  } deriving (Show, Eq)
$(deriveJSON algorandTrainOptions 'TealCompilationResult)
