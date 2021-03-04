-- SPDX-FileCopyrightText: 2021 Serokell <https://serokell.io>
--
-- SPDX-License-Identifier: MPL-2.0

-- | Helper for building transactions.
module Data.Algorand.Transaction.Build
  ( buildTransaction
  , suggestedFee
  ) where

import qualified Data.ByteString.Lazy as BSL

import Data.Algorand.Address (Address)
import Data.Algorand.Amount (Microalgos)
import Data.Algorand.MessagePack (Canonical (Canonical), pack)
import Data.Algorand.Transaction (Transaction (..), TransactionType)
import Network.Algorand.Node.Api (SuggestedParams(..))


-- | Pre-fills transaction header with recommended parameters
-- obtained from the node.
--
-- You can alter the result of this function if you want to fine-tune
-- the transaction, but if you do, use 'suggestedFee' to recalculate the fee
-- taking into account the changes you made.
buildTransaction
  :: SuggestedParams  -- ^ Parameters suggested by the node.
  -> Address  -- ^ Sender of the transaction.
  -> TransactionType  -- ^ Type-specific data.
  -> Transaction
buildTransaction sp@SuggestedParams{..} sender tt = tx1{ tFee = suggestedFee sp tx1 }
  where
    tx1 = Transaction
      { tSender = sender
      , tFee = 0
      , tFirstValid = spLastRound
      , tLastValid = spLastRound + 1000
      , tNote = Nothing
      , tGenesisId = Just spGenesisId
      , tGenesisHash = spGenesisHash
      , tTxType = tt
      , tGroup = Nothing
      , tLease = Nothing
      , tRekeyTo = Nothing
      }

-- | Calculate the recommended fee for this transaction based on its
-- size and current network parameters.
suggestedFee :: SuggestedParams -> Transaction -> Microalgos
suggestedFee SuggestedParams{spFee, spMinFee} txn = max spMinFee (spFee * fromIntegral len)
  where
    len = BSL.length $ pack (Canonical txn)
