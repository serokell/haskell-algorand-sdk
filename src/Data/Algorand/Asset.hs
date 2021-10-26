-- SPDX-FileCopyrightText: 2021 Serokell <https://serokell.io>
--
-- SPDX-License-Identifier: MPL-2.0

-- | Algorand asset.
module Data.Algorand.Asset
  ( AssetIndex
  , Asset (..)
  , AssetParams (..)
  ) where

import Data.Aeson.TH (deriveJSON)
import Data.String (IsString)
import Data.Word (Word64)

import Data.Algorand.Address (Address)
import Data.Algorand.Amount (Microalgos)
import Data.Algorand.MessagePack (MessagePackObject (..), MessageUnpackObject (..), (&), (.:?),
                                  (.=))
import Network.Algorand.Api.Json (algorandTrainOptions)

type AssetIndex = Word64

data Asset = Asset
  { asAmount :: Microalgos
  -- ^ Number of units held.
  , asAssetId :: AssetIndex
  -- ^ Asset ID of the holding.
  , asCreator :: Address
  -- ^ Address that created this asset.
  -- This is the address where the parameters for this asset can be found, and
  -- also the address where unwanted asset units can be sent in the worst case.
  , asIsFrozen :: Bool
  -- ^ Whether or not the holding is frozen.
  } deriving stock (Show, Eq)
$(deriveJSON algorandTrainOptions 'Asset)

data AssetParams = AssetParams
  { acDecimals :: Word64
  -- ^ [dc] The number of digits to use after the decimal point when displaying
  -- this asset. If 0, the asset is not divisible. If 1, the base unit of the
  -- asset is in tenths. If 2, the base unit of the asset is in hundredths,
  -- and so on.
  -- This value must be between 0 and 19 (inclusive).
  , acTotal :: Word64
  -- ^ [t] The total number of units of this asset.
  } deriving stock (Show, Eq)
$(deriveJSON algorandTrainOptions 'AssetParams)

assetParamsFieldName :: IsString s => String -> s
assetParamsFieldName = \case
  "acDecimals" -> "dc"
  "acTotal" -> "t"
  x -> error $ "Unmapped asset params field name: " <> x

instance MessagePackObject AssetParams where
  toCanonicalObject = \case
    AssetParams{..} -> mempty
      & f "acDecimals" .= acDecimals
      & f "acTotal" .= acTotal
    where
      f = assetParamsFieldName

instance MessageUnpackObject AssetParams where
  fromCanonicalObject o = do
    acDecimals <- o .:? f "acDecimals"
    acTotal <- o .:? f "acTotal"
    pure AssetParams{..}
    where
      f = assetParamsFieldName

-- apCreator :: Address
--   -- ^ The address that created this asset.
--   -- This is the address where the parameters for this asset can be found, and
--   -- also the address where unwanted asset units can be sent in the worst case.
--   ,
