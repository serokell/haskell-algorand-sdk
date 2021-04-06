-- SPDX-FileCopyrightText: 2021 Serokell <https://serokell.io>
--
-- SPDX-License-Identifier: MPL-2.0

-- | Algorand block.
module Data.Algorand.Block
  ( Block (..)
  , Rewards (..)
  , UpgradeState (..)
  , UpgradeVote (..)
  , BlockWrapped (..)
  , BlockHash
  , Round (..)
  ) where

import Data.Aeson (ToJSON, FromJSON)
import Data.ByteArray (Bytes)
import Data.Word (Word64)
import GHC.Generics (Generic)
import Data.Text (Text)
import qualified Data.Text as T
import Data.ByteArray.Sized (SizedByteArray)
import Servant.API (ToHttpApiData (toQueryParam))
import Data.Time (UTCTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)

import Data.Algorand.Amount (Microalgos)
import Data.Algorand.MessagePack (AlgoMessagePack (..), MessageUnpackObject (..),  Canonical (..), (.:), (.:?), (.:>))
import Data.Algorand.Transaction
import Data.Algorand.Transaction.Signed (BlockTransaction)

type BlockHash = SizedByteArray 32 Bytes
type Seed = SizedByteArray 32 Bytes
type TransactionsRoot = SizedByteArray 32 Bytes
type Addr = SizedByteArray 32 Bytes

newtype Round = Round { unRound :: Word64 }
  deriving stock (Eq, Ord, Show)
  deriving newtype (Enum, ToJSON, FromJSON)

instance AlgoMessagePack Round where
  toAlgoObject = toAlgoObject . unRound
  fromAlgoObject = fmap Round . fromAlgoObject

instance ToHttpApiData Round where
  toQueryParam (Round r) = T.pack $ show r

data Rewards = Rewards
  { bFeeSink :: Addr
  -- ^ [fees] accepts transaction fees, it can only
  -- spend to the incentive pool.
  , bRewardsCalculationRound :: Microalgos
  -- ^ [rwcalr] number of leftover MicroAlgos after
  -- the distribution of rewards-rate MicroAlgos for
  -- every reward unit in the next round.
  , bRewardsLevel :: Microalgos
  -- ^ [earn] How many rewards, in MicroAlgos, have been
  -- distributed to each RewardUnit of MicroAlgos since genesis.
  , bRewardsPool :: Addr
  -- ^ [rwd] accepts periodic injections from the fee-sink
  -- and continually redistributes them as rewards.
  , bRewardsRate :: Maybe Microalgos
  -- ^ [rate] Number of new MicroAlgos added to the
  -- participation stake from rewards at the next round.
  , bRewardsResidue :: Microalgos
  -- ^ [frac] Number of leftover MicroAlgos after the
  -- distribution of RewardsRate/rewardUnits MicroAlgos
  -- for every reward unit in the next round.
  }
  deriving stock (Eq, Generic, Show)

instance MessageUnpackObject Rewards where
  fromCanonicalObject o = do
    bFeeSink <- o .: "fees"
    bRewardsCalculationRound <- o .: "rwcalr"
    bRewardsLevel <- o .: "earn"
    bRewardsPool <- o .: "rwd"
    bRewardsRate <- o .:? "rate"
    bRewardsResidue <- o .: "frac"
    pure $ Rewards {..}

-- | Fields relating to a protocol upgrade.
data UpgradeState = UpgradeState
  { bCurrentProtocol :: Text
  -- ^ [proto] The current protocol version.
  , bNextProtocol :: Maybe Text
  -- ^ [nextproto] The next proposed protocol version.
  , bNextProtocolApprovals :: Maybe Word64
  -- ^ [nextyes] Number of blocks which approved
  -- the protocol upgrade.
  , bNextProtocolSwitchOn :: Maybe Word64
  -- ^ [nextswitch] Round on which the protocol
  -- upgrade will take effect.
  , bNextProtocolVoteBefore :: Maybe Word64
  -- ^ [nextbefore] Deadline round for this protocol
  -- upgrade (No votes will be consider after this round).
  }
  deriving stock (Eq, Generic, Show)

instance MessageUnpackObject UpgradeState where
  fromCanonicalObject o = do
    bCurrentProtocol <- o .: "proto"
    bNextProtocolVoteBefore <- o .:? "nextbefore"
    bNextProtocol <- o .:? "nextproto"
    bNextProtocolSwitchOn <- o .:? "nextswitch"
    bNextProtocolApprovals <- o .:? "nextyes"
    pure $ UpgradeState {..}

-- | Fields relating to voting for a protocol upgrade.
data UpgradeVote = UpgradeVote
  { bUpgradeApprove :: Maybe Bool
  -- ^ [upgradeyes] Indicates a yes vote for
  -- the current proposal.
  , bUpgradeDelay :: Maybe Word64
  -- ^ [upgradedelay] Indicates the time between
  -- acceptance and execution.
  , bUpgradePropose :: Maybe Text
  -- ^ [upgradeprop] Indicates a proposed upgrade.
  }
  deriving stock (Eq, Generic, Show)

instance MessageUnpackObject UpgradeVote where
  fromCanonicalObject o = do
    bUpgradeApprove <- o .:? "upgradeyes"
    bUpgradeDelay <- o .:? "upgradedelay"
    bUpgradePropose <- o .:? "upgradeprop"
    pure $ UpgradeVote {..}

-- | An Algorand block.
data Block = Block
  { bGenesisHash :: GenesisHash
  -- ^ [gh] hash to which this block belongs.
  , bGenesisId :: Text
  -- ^ [gen] ID to which this block belongs.
  , bPrevBlockHash :: BlockHash
  -- ^ [prev] Previous block hash.
  , bRewards :: Maybe Rewards
  , bRound :: Round
  -- ^ [rnd] Current round on which this block
  -- was appended to the chain.
  , bSeed :: Seed
  -- ^ [seed] Sortition seed.
  , bTimestamp :: UTCTime
  -- ^ [ts] Block creation timestamp in seconds
  -- since epoch.
  , bTransactions :: [BlockTransaction]
  -- ^ [txns] list of transactions corresponding
  -- to a given round.
  , bTransactionsRoot :: TransactionsRoot
  -- ^ [txn] TransactionsRoot authenticates the set of
  -- transactions appearing in the block. More specifically,
  -- it's the root of a merkle tree whose leaves are
  -- the block's Txids, in lexicographic order.
  -- For the empty block, it's 0. Note that the TxnRoot
  -- does not authenticate the signatures on the transactions,
  -- only the transactions themselves. Two blocks with the
  -- same transactions but in a different order and with
  -- different signatures will have the same TxnRoot.
  , bTxnCounter :: Maybe Word64
  -- ^ [tc] TxnCounter counts the number of transactions
  -- committed in the ledger, from the time at which support
  -- for this feature was introduced.
  -- Specifically, TxnCounter is the number of the next
  -- transaction that will be committed after this block.
  -- It is 0 when no transactions have ever been committed
  -- (since TxnCounter started being supported).
  , bUpgradeState :: Maybe UpgradeState
  , bUpgradeVote :: Maybe UpgradeVote
  }
  deriving stock (Eq, Generic, Show)

instance MessageUnpackObject Block where
  fromCanonicalObject o = do
    bGenesisHash <- o .: "gh"
    bGenesisId <- o .: "gen"
    bPrevBlockHash <- o .: "prev"
    bRewards <- fromCanonicalObject o
    bRound <- o .: "rnd"
    bSeed <- o .: "seed"
    ts :: Word64 <- o .: "ts"
    let bTimestamp = posixSecondsToUTCTime (realToFrac ts)
    bTransactions <- map unCanonical <$> (o .:? "txns")
    bTransactionsRoot <- o .: "txn"
    bTxnCounter <- o .:? "tc"
    bUpgradeState <- fromCanonicalObject o
    bUpgradeVote <- fromCanonicalObject o
    pure $ Block {..}

newtype BlockWrapped = BlockWrapped
  { unwrapBlock :: Block
  }
  deriving stock (Eq, Generic, Show)

instance MessageUnpackObject BlockWrapped where
  fromCanonicalObject o = BlockWrapped <$> (o .:> "block")
