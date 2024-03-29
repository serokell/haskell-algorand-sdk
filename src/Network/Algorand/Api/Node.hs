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
import Data.Algorand.Transaction (AppIndex, AssetIndex, GenesisHash, StateSchema)
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

-- @AssetHolding@ in swagger
data Asset = Asset
  { asAmount :: Microalgos
  -- ^ Number of units held.
  , asAssetId :: AssetIndex
  -- ^ Asset ID of the holding.
  , asIsFrozen :: Bool
  -- ^ Whether or not the holding is frozen.
  , asDeleted :: Maybe Bool
  -- ^ Whether or not the asset holding is currently deleted from its account.
  , asOptedInAtRound :: Maybe Round
  -- ^ Round during which the account opted into this asset holding.
  , asOptedOutAtRound :: Maybe Round
  -- ^ Round during which the account opted out of this asset holding.
  }
  deriving stock (Show, Eq)
$(deriveJSON algorandTrainOptions 'Asset)

data LocalState = LocalState
  { lsId :: AppIndex
  -- ^ The application which this local state is for.
  , lsDeleted :: Maybe Bool
  -- ^ Whether or not the application local state is currently deleted from its account.
  , lsOptedInAtRound :: Maybe Round
  -- ^ Round when the account opted into the application.
  , lsOptedOutAtRound :: Maybe Round
  -- ^ Round when account closed out of the application.
  , lsSchema :: StateSchema
  , lsKeyValue :: Maybe TealKeyValueStore
  -- ^ Storage associated with the account and the application.
  } deriving stock (Show, Eq)
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
  , aAppsTotalExtraPages :: Maybe Word64
  , aAppsTotalSchema :: Maybe StateSchema
  , aAssets :: Maybe [Asset]
  -- ^ assets held by this account.
  , aAuthAddr :: Maybe Address
  -- ^ the address against which signing should be checked.
  -- If empty, the address of the current account is used. This field can be
  -- updated in any transaction by setting the RekeyTo field.
  , aClosedAtRound :: Maybe Round
  -- ^ Round during which this account was most recently closed.
  , aCreatedAtRound :: Maybe Round
  -- ^ Round during which this account was most recently closed.
  , aDeleted :: Maybe Bool
  -- ^ whether or not this account is currently closed.
  , aPendingRewards :: Microalgos
  -- ^ amount of MicroAlgos of pending rewards in this account.
  , aRewardBase :: Maybe Microalgos
  -- ^ used as part of the rewards computation. Only applicable to accounts
  -- which are participating.
  , aRewards :: Microalgos
  -- ^ total rewards of MicroAlgos the account has received, including pending rewards.
  , aRound :: Round
  -- ^ the round for which this information is relevant.
  , aSigType :: Text
  -- ^ indicates what type of signature is used by this account
  , aStatus :: Text
  -- ^ delegation status of the account's MicroAlgos
  } deriving (Generic, Show, Eq)
$(deriveJSON algorandTrainOptions 'Account)

newtype TransactionsRep = TransactionsRep
  { trTxId :: Text
  } deriving (Generic, Show)
$(deriveJSON algorandCamelOptions 'TransactionsRep)

data TransactionInfo = TransactionInfo
  { tiApplicationIndex :: Maybe AppIndex
  , tiAssetClosingAmount :: Maybe Microalgos
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
  , spGenesisId :: Network
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
    -- This works only for the last ~1000 blocks
  , _block :: route -- DEPRECATED
      :- "v2"
      :> "blocks"
      :> Capture "round" Round
      :> QueryParam "format" Text
      -- do not try passing format other than msgpack here
      :> Get '[MsgPack] BlockWrapped
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
