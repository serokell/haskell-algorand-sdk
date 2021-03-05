-- SPDX-FileCopyrightText: 2021 Serokell <https://serokell.io>
--
-- SPDX-License-Identifier: MPL-2.0

-- | Tools for working with Algorand tokens.
module Data.Algorand.Amount
  ( Microalgos
  , microAlgos
  ) where

import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.Default.Class (Default)
import Data.Word (Word64)
import GHC.Generics (Generic)

import Data.Algorand.MessagePack (AlgoMessagePack (..), CanonicalZero, NonZeroValue)


-- | Amount of microAlgos.
newtype Microalgos = Microalgos Word64
  deriving (Bounded, CanonicalZero, Default, Enum, Eq, Generic, Integral, NonZeroValue, Num, Ord, Real)
  deriving newtype (Read, Show)

instance AlgoMessagePack Microalgos where
  toAlgoObject (Microalgos a) = toAlgoObject a
  fromAlgoObject = fmap Microalgos . fromAlgoObject

instance ToJSON Microalgos where
  toEncoding (Microalgos w) = toEncoding w
  toJSON (Microalgos w) = toJSON w

instance FromJSON Microalgos where
  parseJSON o = Microalgos <$> parseJSON o


-- | Create an amount of 'Microalgos'.
microAlgos :: Word64 -> Microalgos
microAlgos = Microalgos
