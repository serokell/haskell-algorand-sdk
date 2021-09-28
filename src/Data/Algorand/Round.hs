-- SPDX-FileCopyrightText: 2021 Serokell <https://serokell.io>
--
-- SPDX-License-Identifier: MPL-2.0

-- | Algorand round.
module Data.Algorand.Round
  ( Round (..)
  ) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Word (Word64)
import Servant.API (ToHttpApiData)

import Data.Algorand.MessagePack (AlgoMessagePack (..), CanonicalZero, NonZeroValue)

-- | Algorand round.
newtype Round = Round { unRound :: Word64 }
  deriving stock (Eq, Ord, Show)
  deriving newtype (Enum, Num, ToHttpApiData, ToJSON, FromJSON, CanonicalZero, NonZeroValue)

instance AlgoMessagePack Round where
  toAlgoObject = toAlgoObject . unRound
  fromAlgoObject = fmap Round . fromAlgoObject
