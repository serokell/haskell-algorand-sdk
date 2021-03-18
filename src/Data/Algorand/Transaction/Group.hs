-- SPDX-FileCopyrightText: 2021 Serokell <https://serokell.io>
--
-- SPDX-License-Identifier: MPL-2.0

-- | Helpers for working with transaction groups.
module Data.Algorand.Transaction.Group
  ( getGroupIdFor
  , makeGroup
  , isValidGroup
  ) where

import qualified Data.ByteString.Lazy as BSL

import Crypto.Algorand.Hash (hash32)
import Data.Algorand.MessagePack ((.=), pack, toAlgoObject)
import Data.Algorand.Transaction (Transaction (..), TransactionGroupId, transactionId')
import Data.ByteString (ByteString)
import Data.Function ((&))


-- | Compute the group ID for a list of transactions.
--
-- The order of transactions in the list matters!
-- Make sure all transactions have all their fields filled in, including sender!
getGroupIdFor :: [Transaction] -> TransactionGroupId
getGroupIdFor =
    hash32 . ("TG" <>) . packGroup . map (transactionId' . setGroupId Nothing)
  where
    packGroup :: [ByteString] -> ByteString
    packGroup txids = (BSL.toStrict . pack . toAlgoObject) $ mempty & "txlist" .= txids

-- | Make a list of transactions into a group.
--
-- This computes the correct group ID for this particular list of transactions
-- and then overwrites the 'tGroup' field of each of the transactions with
-- the computed value.
-- Make sure all transactions have all their fields filled in, including sender!
makeGroup :: [Transaction] -> [Transaction]
makeGroup txs = map (setGroupId $ Just (getGroupIdFor txs)) txs


-- | Check whether the transactions in the list for a valid group.
--
-- Namely: all transactions have to be part of the same group
-- (have the same 'tGroup'), the group must be complete (i.e. the
-- list must contain all transactions from the group), and the list
-- must be in the right order.
isValidGroup :: [Transaction] -> Bool
isValidGroup txs =
  all (\Transaction{tGroup} -> tGroup == Just (getGroupIdFor txs)) txs


-- | Helper (because we do not have lenses (yet)).
setGroupId :: Maybe TransactionGroupId -> Transaction -> Transaction
setGroupId gid tx = tx { tGroup = gid }
