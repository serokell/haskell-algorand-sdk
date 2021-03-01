-- SPDX-FileCopyrightText: 2021 Serokell <https://serokell.io>
--
-- SPDX-License-Identifier: MPL-2.0

-- | Hashing used in Algorand
module Crypto.Algorand.Hash
  ( hash32
  ) where

import Crypto.Hash (hash)
import Crypto.Hash.Algorithms (SHA512t_256)
import Data.ByteArray (ByteArray, ByteArrayAccess, convert)
import Data.ByteArray.Sized (SizedByteArray, unsafeSizedByteArray)


-- | Hashing used in Algorand as a checksum.
--
-- All sort of 32-byte IDs are generated using this function.
-- It is simply SHA-512/256.
hash32 :: (ByteArrayAccess bs, ByteArray out) => bs -> SizedByteArray 32 out
hash32 = unsafeSizedByteArray . convert . hash @_ @SHA512t_256
