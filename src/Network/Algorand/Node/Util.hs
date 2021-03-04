-- SPDX-FileCopyrightText: 2021 Serokell <https://serokell.io>
--
-- SPDX-License-Identifier: MPL-2.0

-- | Convenience wrappers around the node API.
module Network.Algorand.Node.Util
  ( TransactionStatus (..)
  , transactionStatus
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Word (Word64)

import Network.Algorand.Node.Api (TransactionInfo (..))


-- | Status of a transaction in the pool.
data TransactionStatus
  = Waiting  -- ^ Still in the pool waiting to be confirmed.
  | Confirmed Word64  -- ^ Transaction was confirmed at this round.
  | KickedOut Text  -- ^ It was kicked out of this node’s pool for this reason.

-- | Summarise 'TransactionInfo' as 'TransactionStatus'.
transactionStatus :: TransactionInfo -> TransactionStatus
transactionStatus TransactionInfo{tiConfirmedRound, tiPoolError} =
  case tiConfirmedRound of
    Just r -> Confirmed r
    Nothing -> case T.null tiPoolError of
      False -> KickedOut tiPoolError
      True -> Waiting
