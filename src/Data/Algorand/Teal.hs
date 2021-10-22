-- SPDX-FileCopyrightText: 2021 Serokell <https://serokell.io>
--
-- SPDX-License-Identifier: MPL-2.0

-- | Types and helpers related to Transaction Execution Approval Language (TEAL).
module Data.Algorand.Teal
  ( TealCode (..)
  , TealCompilationResult (..)
  , TealKeyValue (..)
  , TealKeyValueStore
  , TealValue (..)

  , tealValueBytesType
  , tealValueUintType
  ) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Aeson.TH (deriveJSON)
import Data.ByteString (ByteString)
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

-- | Represents a key-value pair in an application store.
data TealKeyValue = TealKeyValue
  { tkeKey :: ByteString
  , tkeValue :: TealValue
  } deriving stock (Show, Eq)
$(deriveJSON algorandTrainOptions 'TealKeyValue)

-- | Represents a key-value store for use in an application.
type TealKeyValueStore = [TealKeyValue]

newtype TealCode = TealCode
  { unTealCode :: ByteString
  } deriving newtype (FromJSON, ToJSON)

data TealCompilationResult = TealCompilationResult
  { tcrHash :: Address
  , tcrResult :: TealCode
  }
$(deriveJSON algorandTrainOptions 'TealCompilationResult)
