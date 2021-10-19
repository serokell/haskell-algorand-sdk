-- SPDX-FileCopyrightText: 2021 Serokell <https://serokell.io>
--
-- SPDX-License-Identifier: MPL-2.0

-- | The REST API v2 of the Algorand node.
--
-- See <https://developer.algorand.org/docs/reference/rest-apis/algod/v2/>
module Network.Algorand.Api.Node
  ( BuildVersion (..)
  , Version (..)
  , Account (..)
  , TransactionsRep (..)
  , TransactionInfo (..)
  , SuggestedParams (..)
  , NanoSec (..)
  , NodeStatus (..)
  , Asset (..)
  , LocalState (..)
  , NodeApi (..)
  ) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Aeson.TH (deriveJSON)
import Data.ByteString (ByteString)
import Data.Int (Int64)
import Data.Text (Text)
import Data.Word (Word64)
import GHC.Generics (Generic)
import Servant.API (Capture, Get, JSON, PlainText, Post, QueryParam, ReqBody, (:>))
import Servant.API.Generic ((:-))

import Data.Algorand.Address (Address)
import Data.Algorand.Amount (Microalgos)
import Data.Algorand.Block (BlockWrapped)
import Data.Algorand.Round (Round)
import Data.Algorand.Teal (TealCompilationResult, TealKeyValueStore)
import Data.Algorand.Transaction (AppIndex, AssetIndex, GenesisHash)
import Data.Algorand.Transaction.Signed (SignedTransaction)
import Network.Algorand.Api.Content (Binary, MsgPack)
import Network.Algorand.Api.Json (algorandCamelOptions, algorandSnakeOptions, algorandTrainOptions)
import Network.Algorand.Definitions (Network)

----------------
-- Types
----------------

-- | Node software build version information.
data BuildVersion = BuildVersion
  { bvBranch :: Text
  , bvBuildNumber :: Int64
  , bvChannel :: Text
  , bvCommitHash :: Text
  , bvMajor :: Int64
  , bvMinor :: Int64
  } deriving (Generic, Show)
$(deriveJSON algorandSnakeOptions 'BuildVersion)

-- | algod version information.
data Version = Version
  { vBuild :: BuildVersion
  , vGenesisHashB64 :: Text
  , vGenesisId :: Network
  , vVersions :: [Text]
  } deriving (Generic, Show)
$(deriveJSON algorandSnakeOptions 'Version)

newtype NanoSec = NanoSec { unNanoSec :: Word64 }
  deriving stock (Eq, Show, Ord)
  deriving newtype (Enum, Integral, Num, Real, FromJSON, ToJSON)

data NodeStatus = NodeStatus
  { nsCatchpoint :: Maybe Text
  -- ^ The current catchpoint that is being caught up to
  , nsCatchpointAcquiredBlocks :: Maybe Word64
  -- ^ The number of blocks that have already been
  -- obtained by the node as part of the catchup
  , nsCatchpointProcessedAccounts :: Maybe Word64
  -- ^ The number of accounts from the current catchpoint
  -- that have been processed so far as part of the catchup
  , nsCatchpointTotalAccounts :: Maybe Word64
  -- ^ The total number of accounts included in
  -- the current catchpoint
  , nsCatchpointTotalBlocks :: Maybe Word64
  -- ^ The total number of blocks that are required to complete
  -- the current catchpoint catchup
  , nsCatchpointVerifiedAccounts :: Maybe Word64
  -- ^ The number of accounts from the current catchpoint that
  -- have been verified so far as part of the catchup
  , nsCatchupTime :: NanoSec
  , nsLastCatchpoint :: Maybe Text
  -- ^ The last catchpoint seen by the node
  , nsLastRound :: Round
  -- ^ The last round seen
  , nsLastVersion :: Text
  -- ^ indicates the last consensus version supported
  , nsNextVersion :: Text
  -- ^ the next version of consensus protocol to use
  , nsNextVersionRound :: Round
  -- ^ round at which the next consensus version will apply
  , nsNextVersionSupported :: Bool
  -- ^ indicates whether the next consensus version
  -- is supported by this node
  , nsStoppedAtUnsupportedRound :: Bool
  -- ^ indicates that the node does not support
  -- the new rounds and has stopped making progress
  , nsTimeSinceLastRound :: NanoSec
  }
$(deriveJSON algorandTrainOptions 'NodeStatus)

newtype TransactionsRep = TransactionsRep
  { trTxId :: Text
  } deriving (Generic, Show)
$(deriveJSON algorandCamelOptions 'TransactionsRep)

data Asset = Asset
  { asAmount :: Microalgos
  -- ^ Number of units held.
  , asAssetId :: AssetIndex
  -- ^ Asset ID of the holding.
  , asCreator :: Address
  -- ^ Address that created this asset.
  -- This is the address where the parameters for this asset can be found, and
  -- also the address where unwanted asset units can be sent in the worst case.
  , asIsFrozen :: Bool
  -- ^ Whether or not the holding is frozen.
  }
  deriving stock Show
$(deriveJSON algorandTrainOptions 'Asset)

data LocalState = LocalState
  { lsId :: AppIndex
  -- ^ The application which this local state is for.
  , lsKeyValue :: Maybe TealKeyValueStore
  -- ^ Storage associated with the account and the application.
  } deriving stock Show
$(deriveJSON algorandTrainOptions 'LocalState)

data Account = Account
  { aAddress :: Address
  -- ^ the account public key.
  , aAmount :: Microalgos
  -- ^ total number of MicroAlgos in the account.
  , aAmountWithoutPendingRewards :: Microalgos
  -- ^ specifies the amount of MicroAlgos in the account, without the pending rewards.
  , aAppsLocalState :: Maybe [LocalState]
  -- ^ applications local data stored in this account.
  , aAssets :: Maybe [Asset]
  -- ^ assets held by this account.
  , aAuthAddr :: Maybe Address
  -- ^ the address against which signing should be checked.
  -- If empty, the address of the current account is used. This field can be
  -- updated in any transaction by setting the RekeyTo field.
  , aPendingRewards :: Microalgos
  -- ^ amount of MicroAlgos of pending rewards in this account.
  , aRewardBase :: Maybe Microalgos
  -- ^ used as part of the rewards computation. Only applicable to accounts
  -- which are participating.
  , aRewards :: Microalgos
  -- ^ total rewards of MicroAlgos the account has received, including pending rewards.
  , aRound :: Round
  -- ^ the round for which this information is relevant.
  , aStatus :: Text
  -- ^ delegation status of the account's MicroAlgos
  } deriving (Generic, Show)
$(deriveJSON algorandTrainOptions 'Account)

data TransactionInfo = TransactionInfo
  { tiApplicationIndex :: Maybe AppIndex
  , tiAssetIndex :: Maybe AssetIndex
  , tiCloseRewards :: Maybe Microalgos
  , tiClosingAmount :: Maybe Microalgos
  , tiConfirmedRound :: Maybe Round
  , tiPoolError :: Text
  , tiReceiverRewards :: Maybe Microalgos
  , tiSenderRewards :: Maybe Microalgos
  , tiTxn :: SignedTransaction
  }
$(deriveJSON algorandTrainOptions 'TransactionInfo)

data SuggestedParams = SuggestedParams
  { spConsensusVersion :: Text
  , spFee :: Microalgos
  , spGenesisHash :: GenesisHash
  , spGenesisId :: Text
  , spLastRound :: Round
  , spMinFee :: Microalgos
  }
$(deriveJSON algorandTrainOptions 'SuggestedParams)

----------------
-- API
----------------

-- | Algod Node API
data NodeApi route = NodeApi
  { _version :: route
      :- "versions"
      :> Get '[JSON] Version
  , _status :: route
      :- "v2"
      :> "status"
      :> Get '[JSON] NodeStatus
  , _block :: route
      :- "v2"
      :> "blocks"
      :> Capture "round" Round
      :> QueryParam "format" Text
      -- do not try passing format other than msgpack here
      :> Get '[MsgPack] BlockWrapped
  , _account :: route
      :- "v2"
      :> "accounts"
      :> Capture "address" Address
      :> Get '[JSON] Account
  , _transactions :: route
      :- "v2"
      :> "transactions"
      :> ReqBody '[Binary] [SignedTransaction]
      :> Post '[JSON] TransactionsRep
  , _transactionsRaw :: route
      :- "v2"
      :> "transactions"
      :> ReqBody '[Binary] ByteString
      :> Post '[JSON] TransactionsRep
  , _transactionsPending :: route
      :- "v2"
      :> "transactions"
      :> "pending"
      :> Capture "txId" Text
      :> Get '[JSON] TransactionInfo
  , _transactionsParams :: route
      :- "v2"
      :> "transactions"
      :> "params"
      :> Get '[JSON] SuggestedParams
  , _compileTeal :: route
      :- "v2"
      :> "teal"
      :> "compile"
      :> ReqBody '[PlainText] Text
      :> Post '[JSON] TealCompilationResult
  } deriving (Generic)
