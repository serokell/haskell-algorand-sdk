-- SPDX-FileCopyrightText: 2021 Serokell <https://serokell.io>
--
-- SPDX-License-Identifier: MPL-2.0

-- | Tools for working with Algorand tokens.
module Data.Algorand.Amount
  ( Microalgos
  , microAlgos
  ) where

import Data.Default.Class (Default)
import Data.Word (Word64)
import GHC.Generics (Generic)
import Data.MessagePack (MessagePack (fromObject, toObject))

import Data.Algorand.MessagePack (CanonicalZero, NonZeroValue)


-- | Amount of microAlgos.
newtype Microalgos = Microalgos Word64
  deriving (Bounded, CanonicalZero, Default, Enum, Eq, Generic, Integral, NonZeroValue, Num, Ord, Real, Show)

instance MessagePack Microalgos where
  toObject (Microalgos a) = toObject a
  fromObject = fmap Microalgos . fromObject


-- | Create an amount of 'Microalgos'.
microAlgos :: Word64 -> Microalgos
microAlgos = Microalgos
