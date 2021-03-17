-- SPDX-FileCopyrightText: 2021 Serokell <https://serokell.io>
--
-- SPDX-License-Identifier: MPL-2.0

-- | The REST API v2 of Algod, the Algorand node.
--
-- See <https://developer.algorand.org/docs/reference/rest-apis/algod/v2/>
module Network.Algorand.Node.Api.Type
  ( BuildVersion (..)
  , Version (..)
  , Account (..)
  , TransactionsRep (..)
  , TransactionInfo (..)
  , SuggestedParams (..)
  , NanoSec (..)
  , NodeStatus (..)
  , TealCode (..)
  , TealCompilationResult (..)
  , Asset (..)
  , TealValue (..)
  , TealKvEntry (..)
  , TealKvStore
  , LocalState (..)
  , tealValueBytesType
  , tealValueUintType
  ) where

import GHC.Generics (Generic)

import Data.Aeson (FromJSON, ToJSON)
import Data.Aeson.TH (deriveJSON)
import Data.ByteString (ByteString)
import Data.Int (Int64)
import Data.Text (Text)
import Data.Word (Word64)

import Data.Algorand.Address (Address)
import Data.Algorand.Amount (Microalgos)
import Data.Algorand.Transaction (AppIndex, AssetIndex, GenesisHash)
import Data.Algorand.Transaction.Signed (SignedTransaction)
import Data.Algorand.Block
import Network.Algorand.Node.Api.Json (algorandCamelOptions, algorandSnakeOptions, algorandTrainOptions)

-- | Node software build version information.
data BuildVersion = BuildVersion
  { bvBranch :: Text
  , bvBuildNumber :: Int64
  , bvChannel :: Text
  , bvCommitHash :: Text
  , bvMajor :: Int64
  , bvMinor :: Int64
  }
  deriving (Generic, Show)
$(deriveJSON algorandSnakeOptions 'BuildVersion)

-- | algod version information.
data Version = Version
  { vBuild :: BuildVersion
  , vGenesisHashB64 :: Text
  , vGenesisId :: Text
  , vVersions :: [Text]
  }
  deriving (Generic, Show)
$(deriveJSON algorandSnakeOptions 'Version)

data TransactionsRep = TransactionsRep
  { trTxId :: Text
  }
  deriving (Generic, Show)
$(deriveJSON algorandCamelOptions 'TransactionsRep)

data Asset = Asset
  { asAmount :: Word64
  -- ^ Number of units held.
  , asAssetId :: AssetIndex
  -- ^ Asset ID of the holding.
  , asCreator :: Address
  -- ^ Address that created this asset.
  -- This is the address where the parameters for this
  -- asset can be found, and also the address where
  -- unwanted asset units can be sent in the worst case.
  , asDeleted :: Maybe Bool
  -- ^ Whether or not the asset holding is
  -- currently deleted from its account.
  , asIsFrozen :: Bool
  -- ^ Whether or not the holding is frozen.
  , asOptedInAtRound :: Maybe Round
  -- ^ Round during which the account opted
  -- into this asset holding.
  , asOptedOutAtRound :: Maybe Round
  }
  deriving stock Show
$(deriveJSON algorandTrainOptions 'Asset)

data TealValue = TealValue
  { tvBytes :: ByteString
  , tvUint :: Word64
  , tvType :: Word64
  }
  deriving stock Show
$(deriveJSON algorandTrainOptions 'TealValue)

tealValueBytesType :: Word64
tealValueBytesType = 1

tealValueUintType :: Word64
tealValueUintType = 2

data TealKvEntry = TealKvEntry
  { tkeKey :: ByteString
  , tkeValue :: TealValue
  }
  deriving stock Show
$(deriveJSON algorandTrainOptions 'TealKvEntry)

type TealKvStore = [TealKvEntry]

data LocalState = LocalState
  { lsClosedOutAtRound :: Maybe Round
  -- ^ Round when account closed out of the application.
  , lsDeleted :: Maybe Bool
  -- ^ Whether or not the application local state
  -- is currently deleted from its account.
  , lsId :: AppIndex
  -- ^ The application which this local state is for.
  , lsKeyValue :: Maybe TealKvStore
  -- ^ Storage associated with the account and
  -- the application.

  , lsOptedInAtRound :: Maybe Round
  -- ^ Round when the account opted
  -- into the application.

  -- , schema :: ApplicationStateSchema
  }
  deriving stock Show
$(deriveJSON algorandTrainOptions 'LocalState)

data Account = Account
  { aAddress :: Address
  , aAmount :: Microalgos
  , aAmountWithoutPendingRewards :: Microalgos
  , aAppsLocalState :: Maybe [LocalState]
  --, aAppsTotalSchema :: Maybe StateSchema
  , aAssets :: Maybe [Asset]
  , aAuthAddr :: Maybe Address
  --, aCreatedApps :: Maybe
  --, aCreatedAssets :: Maybe
  --, aAccuntParticipation :: Maybe
  , aPendingRewards :: Microalgos
  , aRewardBase :: Maybe Microalgos
  , aRewards :: Microalgos
  , aRound :: Word64
  --, aSigType :: Maybe
  , aStatus :: Text
  }
  deriving (Generic, Show)
$(deriveJSON algorandTrainOptions 'Account)

data TransactionInfo = TransactionInfo
  { tiApplicationIndex :: Maybe AppIndex
  , tiAssetIndex :: Maybe AssetIndex
  , tiCloseRewards :: Maybe Microalgos
  , tiClosingAmount :: Maybe Microalgos
  , tiConfirmedRound :: Maybe Word64
-- TODO:
--  , tiGlobalStateDelta :: Maybe ...
--  , tiLocalStateDelta :: Maybe ...
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
  , spLastRound :: Word64
  , spMinFee :: Microalgos
  }
$(deriveJSON algorandTrainOptions 'SuggestedParams)

newtype NanoSec = NanoSec { unNanoSec :: Integer }
  deriving stock (Eq, Ord, Show)
  deriving newtype (ToJSON, FromJSON)

data NodeStatus = NodeStatus
  { nsCatchupTime :: NanoSec
  , nsLastRound :: Round
  , nsLastVersion :: Text
  -- ^ indicates the last consensus version supported
  , nsNextVersion :: Text
  -- ^ NextVersion of consensus protocol to use
  , nsNextVersionRound :: Round
  -- ^ round at which the next consensus version will apply
  , nsNextVersionSupported :: Bool
  -- ^ indicates whether the next consensus version
  -- is supported by this node
  , nsStoppedAtUnsupportedRound :: Bool
  -- ^ indicates that the node does not supports
  -- the new rounds and has stopped making progress
  , nsTimeSinceLastRound :: NanoSec
  , nsCatchpoint :: Maybe Text
  -- ^ The current catchpoint that is being caught up to
  , nsCatchpointAcquiredBlocks :: Maybe Integer
  -- ^ The number of blocks that have already been
  -- obtained by the node as part of the catchup
  , nsCatchpointProcessedAccounts :: Maybe Integer
  -- ^ The number of accounts from the current catchpoint
  -- that have been processed so far as part of the catchup
  , nsCatchpointTotalAccounts :: Maybe Integer
  -- ^ The total number of accounts included in
  -- the current catchpoint
  , nsCatchpointTotalBlocks :: Maybe Integer
  -- ^ The total number of blocks that are required to complete
  -- the current catchpoint catchup
  , nsCatchpointVerifiedAccounts :: Maybe Integer
  -- ^ The number of accounts from the current catchpoint that
  -- have been verified so far as part of the catchup
  , nsLastCatchpoint :: Maybe Text
  -- ^ The last catchpoint seen by the node
  }
$(deriveJSON algorandTrainOptions 'NodeStatus)

newtype TealCode = TealCode
  { unTealCode :: ByteString }
  deriving newtype (FromJSON, ToJSON)

data TealCompilationResult = TealCompilationResult
  { tcrHash :: Address
  , tcrResult :: TealCode
  }
$(deriveJSON algorandTrainOptions 'TealCompilationResult)
