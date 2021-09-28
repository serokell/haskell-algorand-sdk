-- SPDX-FileCopyrightText: 2021 Serokell <https://serokell.io>
--
-- SPDX-License-Identifier: MPL-2.0

module Test.Data.Algorand.Program where

import Data.ByteString (ByteString)

import Hedgehog (MonadGen)
import qualified Hedgehog.Gen as G
import qualified Hedgehog.Range as R


-- | Generate a compiled TEAL program.
--
-- FIXME: Currently, this just generates arbitrary (non-empty) bytes.
genProgram :: MonadGen m => m ByteString
genProgram = G.bytes (R.linear 1 1000)
