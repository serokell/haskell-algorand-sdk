-- SPDX-FileCopyrightText: 2021 Serokell <https://serokell.io>
--
-- SPDX-License-Identifier: MPL-2.0

-- | The REST API v2 of Algod, the Algorand node.
--
-- See <https://developer.algorand.org/docs/reference/rest-apis/algod/v2/>
module Network.Algorand.Node.Api
  ( Api (..)
  , ApiAny (..)
  , ApiV2 (..)

  , BuildVersion (..)
  , Version (..)
  , Account (..)
  , TransactionsRep (..)
  , TransactionInfo (..)
  , SuggestedParams (..)
  , NanoSec (..)
  , NodeStatus (..)
  , msgPackFormat

  , TealCode (..)
  , TealCompilationResult (..)
  ) where

import GHC.Generics (Generic)

import qualified Data.Binary as Binary
import Data.Aeson (FromJSON, ToJSON)
import Data.Aeson.TH (deriveJSON)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as BSL
import Data.Int (Int64)
import Data.Text (Text)
import Data.Word (Word64)
import Network.HTTP.Media ((//))
import Servant.API ((:>), Capture, Get, JSON, Post, ReqBody, QueryParam)
import qualified Servant.API.ContentTypes as Mime
import Servant.API.Generic (ToServantApi, (:-))

import qualified Data.Algorand.MessagePack as MP
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

data Account = Account
  { aAddress :: Address
  , aAmount :: Microalgos
  , aAmountWithoutPendingRewards :: Microalgos
  --, aAppsLocalState :: Maybe StateSchema
  --, aAppsTotalSchema :: Maybe StateSchema
  --, aAssets :: Maybe
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

-- | The part of the API that does not depend on the version.
data ApiAny route = ApiAny
  { _health :: route
      :- "health"
      :> Get '[JSON] ()
  , _version :: route
      :- "versions"
      :> Get '[JSON] Version
  }
  deriving (Generic)

-- | Algod API (v2 only).
data ApiV2 route = ApiV2
  { _account :: route
      :- "accounts"
      :> Capture "address" Address
      :> Get '[JSON] Account
  , _transactions :: route
      :- "transactions"
      :> ReqBody '[Binary] [SignedTransaction]
      :> Post '[JSON] TransactionsRep
  , _transactionsRaw :: route
      :- "transactions"
      :> ReqBody '[Binary] ByteString
      :> Post '[JSON] TransactionsRep
  , _transactionsPending :: route
      :- "transactions"
      :> "pending"
      :> Capture "txId" Text
      :> Get '[JSON] TransactionInfo
  , _transactionsParams :: route
      :- "transactions"
      :> "params"
      :> Get '[JSON] SuggestedParams
  , _status :: route
      :- "status"
      :> Get '[JSON] NodeStatus
  , _block :: route
      :- "blocks"
      :> Capture "round" Round
      :> QueryParam "format" Text
      :> Get '[MsgPack] BlockWrapped
  , _compileTeal :: route
      :- "teal"
      :> "compile"
      :> ReqBody '[Mime.PlainText] Text
      :> Post '[JSON] TealCompilationResult
  }
  deriving (Generic)

msgPackFormat :: Maybe Text
msgPackFormat = Just "msgpack"

-- | Algod API.
data Api route = Api
  { _vAny :: route
      :- ToServantApi ApiAny
  , _v2 :: route
      :- "v2"
      :> ToServantApi ApiV2
  }
  deriving (Generic)

{-
 - Utils
 -}

-- | Content type for the `v2/transactions` endpoint.
data Binary
instance Mime.Accept Binary where
  contentType _ = "application" // "x-binary"
instance Mime.MimeRender Binary ByteString where
  mimeRender _ = BSL.fromStrict
instance Mime.MimeRender Binary BSL.ByteString where
  mimeRender _ = id
instance Mime.MimeRender Binary [SignedTransaction] where
  mimeRender _ = mconcat . map (MP.pack . MP.Canonical)

-- | Content type for the `v2/blocks` endpoint.
data MsgPack
instance Mime.Accept MsgPack where
  contentType _ = "application" // "msgpack"
instance Mime.MimeUnrender MsgPack BlockWrapped where
  mimeUnrender _ = parseMsgPack
    where
      eitherToM (Left  (_, _, msg)) = fail msg
      eitherToM (Right (_, _, res)) = pure res
      parseMsgPack bs = MP.runEitherError $
        eitherToM (Binary.decodeOrFail bs)
          >>= MP.fromAlgoObject . MP.unCompatObject
          >>= MP.fromCanonicalObject
