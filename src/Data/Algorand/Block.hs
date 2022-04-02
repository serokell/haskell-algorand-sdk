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
  , TransactionsRoot
  , Seed
  ) where

import Data.Aeson.TH (deriveJSON)
import Data.ByteArray (Bytes)
import Data.ByteArray.Sized (SizedByteArray)
import Data.Text (Text)
import Data.Time (UTCTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Data.Word (Word64)
import GHC.Generics (Generic)

import Data.Algorand.Address (Address)
import Data.Algorand.Amount (Microalgos)
import Data.Algorand.MessagePack (Canonical (..), MessageUnpackObject (..), (.:), (.:>), (.:?))
import Data.Algorand.Round (Round)
import Data.Algorand.Transaction (GenesisHash)
import Data.Algorand.Transaction.Signed (BlockTransaction)
import Network.Algorand.Api.Json (algorandTrainOptions)
import Network.Algorand.Definitions (Network)

type BlockHash = SizedByteArray 32 Bytes
type Seed = SizedByteArray 32 Bytes
type TransactionsRoot = SizedByteArray 32 Bytes

data Rewards = Rewards
  { bFeeSink :: Address
  -- ^ [fees] accepts transaction fees, it can only spend to the incentive pool.
  , bRewardsLevel :: Microalgos
  -- ^ [earn] How many rewards, in MicroAlgos, have been distributed to each
  -- RewardUnit of MicroAlgos since genesis.
  , bRewardsPool :: Address
  -- ^ [rwd] accepts periodic injections from the fee-sink and continually
  -- redistributes them as rewards.
  , bRewardsRate :: Microalgos
  -- ^ [rate] Number of new MicroAlgos added to the participation stake from
  -- rewards at the next round.
  , bRewardsResidue :: Microalgos
  -- ^ [frac] Number of leftover MicroAlgos after the distribution of
  -- RewardsRate/rewardUnits MicroAlgos for every reward unit in the next round.
  , bRewardsCalculationRound :: Round
  -- ^ [rwcalr] the round at which the RewardsRate will be recalculated.
  } deriving stock (Eq, Generic, Show)
$(deriveJSON algorandTrainOptions 'Rewards)

instance MessageUnpackObject Rewards where
  fromCanonicalObject o = do
    bFeeSink <- o .: "fees"
    bRewardsCalculationRound <- o .: "rwcalr"
    bRewardsLevel <- o .:? "earn"
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
  -- ^ [nextyes] Number of blocks which approved the protocol upgrade.
  , bNextProtocolVoteBefore :: Maybe Word64
  -- ^ [nextbefore] specify the last voting round for the next protocol proposal.
  -- If there is no voting for an upgrade taking place, this would be zero.
  , bNextProtocolSwitchOn :: Maybe Word64
  -- ^ [nextswitch] specify the round number at which the next protocol would be
  -- adopted. If there is no upgrade taking place, nor a wait for the next
  -- protocol, this would be zero.
  } deriving stock (Eq, Generic, Show)
$(deriveJSON algorandTrainOptions 'UpgradeState)

instance MessageUnpackObject UpgradeState where
  fromCanonicalObject o = do
    bCurrentProtocol <- o .: "proto"
    bNextProtocol <- o .:? "nextproto"
    bNextProtocolApprovals <- o .:? "nextyes"
    bNextProtocolVoteBefore <- o .:? "nextbefore"
    bNextProtocolSwitchOn <- o .:? "nextswitch"
    pure $ UpgradeState {..}

-- | Fields relating to voting for a protocol upgrade.
data UpgradeVote = UpgradeVote
  {  bUpgradePropose :: Maybe Text
  -- ^ [upgradeprop] Indicates a proposed upgrade.
  , bUpgradeDelay :: Maybe Word64
  -- ^ [upgradedelay] Indicates the time between acceptance and execution.
  , bUpgradeApprove :: Maybe Bool
  -- ^ [upgradeyes] Indicates a yes vote for the current proposal.
  } deriving stock (Eq, Generic, Show)
$(deriveJSON algorandTrainOptions 'UpgradeVote)

instance MessageUnpackObject UpgradeVote where
  fromCanonicalObject o = do
    bUpgradePropose <- o .:? "upgradeprop"
    bUpgradeDelay <- o .:? "upgradedelay"
    bUpgradeApprove <- o .:? "upgradeyes"
    pure $ UpgradeVote {..}

-- | An Algorand block.
data Block = Block
  { bGenesisHash :: GenesisHash
  -- ^ [gh] hash to which this block belongs.
  , bGenesisId :: Network
  -- ^ [gen] ID to which this block belongs.
  , bPrevBlockHash :: BlockHash
  -- ^ [prev] hash of the previous block.
  , bRewards :: Maybe Rewards
  , bRound :: Round
  -- ^ [rnd] Current round on which this block was appended to the chain.
  , bSeed :: Seed
  -- ^ [seed] Sortition seed.
  , bTimestamp :: UTCTime
  -- ^ [ts] Block creation timestamp in seconds since epoch.
  , bTransactions :: [BlockTransaction]
  -- ^ [txns] list of transactions corresponding
  -- to a given round.
  , bTransactionsRoot :: Maybe TransactionsRoot
  -- ^ [txn] TransactionsRoot authenticates the set of transactions appearing
  -- in the block. More specifically, it's the root of a merkle tree whose
  -- leaves are the block's Txids, in lexicographic order.
  -- For the empty block, it's 0. Note that the TxnRoot does not authenticate
  -- the signatures on the transactions, only the transactions themselves.
  -- Two blocks with the same transactions but in a different order and with
  -- different signatures will have the same TxnRoot.
  , bTxnCounter :: Maybe Word64
  -- ^ [tc] TxnCounter counts the number of transactions committed in the
  -- ledger, from the time at which support for this feature was introduced.
  -- Specifically, this counter is the number of the next transaction that will
  -- be committed after this block.
  -- It is 0 when no transactions have ever been committed (since counter
  -- started being supported).
  , bUpgradeState :: Maybe UpgradeState
  , bUpgradeVote :: Maybe UpgradeVote
  } deriving stock (Eq, Generic, Show)

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
    bTransactionsRoot <- o .:? "txn"
    bTxnCounter <- o .:? "tc"
    bUpgradeState <- fromCanonicalObject o
    bUpgradeVote <- fromCanonicalObject o
    pure $ Block {..}

newtype BlockWrapped = BlockWrapped
  { unwrapBlock :: Block
  } deriving stock (Eq, Generic, Show)

instance MessageUnpackObject BlockWrapped where
  fromCanonicalObject o = BlockWrapped <$> (o .:> "block")
