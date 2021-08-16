-- SPDX-FileCopyrightText: 2021 Serokell <https://serokell.io>
--
-- SPDX-License-Identifier: MPL-2.0

-- | Types which describe our Indexer API
-- See <https://developer.algorand.org/docs/reference/rest-apis/indexer/>
module Network.Algorand.Api.Indexer
  ( IndexerApi (..)
  , AccountData (..)
  , ApplicationLocalState (..)
  , AssetHolding (..)
  , BlockResp (..)
  , IdxAccountResponse (..)
  , TransactionResp (..)
  ) where

import qualified Data.Text as T

import Data.Aeson (FromJSON (..), KeyValue ((.=)), ToJSON (..), object, withObject, (.:), (.:?))
import Data.Aeson.TH (deriveJSON)
import Data.ByteString (ByteString)
import Data.Text (Text)
import Data.Time.Clock.POSIX (POSIXTime)
import Data.Word (Word64)
import GHC.Generics (Generic)
import Servant.API (Capture, Get, JSON, QueryParam, (:>))
import Servant.API.Generic ((:-))

import Crypto.Algorand.Signature (SignatureType (..))
import Data.Algorand.Address (Address)
import Data.Algorand.Amount (Microalgos)
import Data.Algorand.Block (BlockHash, Rewards (..), Seed, TransactionsRoot, UpgradeState (..),
                            UpgradeVote (..))
import Data.Algorand.Round (Round)
import Data.Algorand.Teal (TealKeyValueStore)
import Data.Algorand.Transaction (AppIndex, AssetIndex, GenesisHash, Lease, TransactionGroupId,
                                  TransactionType (..))
import Network.Algorand.Api.Json (algorandTrainOptions)

-- | Stores local state associated with an application.
data ApplicationLocalState = ApplicationLocalState
  { lsId :: AppIndex
  -- ^ The application which this local state is for.
  , lsKeyValue :: Maybe TealKeyValueStore
  -- ^ Storage associated with the account and the application.
  } deriving stock Show
$(deriveJSON algorandTrainOptions 'ApplicationLocalState)

-- | Describes an asset held by an account.
data AssetHolding = AssetHolding
  { ahAmount :: Word64
  -- ^ Number of units held.
  , ahAssetId :: AssetIndex
  -- ^ Asset ID of the holding.
  , ahCreator :: Address
  -- ^ Address that created this asset.
  -- This is the address where the parameters for this asset can be found, and
  -- also the address where unwanted asset units can be sent in the worst case.
  , ahIsFrozen :: Bool
  -- ^ Whether or not the holding is frozen.
  , ahDeleted :: Bool
  -- ^ Whether or not this asset is currently deleted.
  , ahOptedInAtRound :: Round
  -- ^ Round during which the account opted into this asset holding.
  , ahOptedOutAtRound :: Maybe Round
  -- ^ Round during which the account opted out of this asset holding.
  } deriving stock Show
$(deriveJSON algorandTrainOptions 'AssetHolding)

-- | Account information at a given round.
data AccountData = AccountData
  { adAddress :: Address
  -- ^ the account public key.
  , adAmount :: Microalgos
  -- ^ total number of MicroAlgos in the account.
  , adAmountWithoutPendingRewards :: Microalgos
  -- ^ specifies the amount of MicroAlgos in the account, without the pending rewards.
  , adAppsLocalState :: Maybe [ApplicationLocalState]
  -- ^ applications local data stored in this account.
  , adAssets :: Maybe [AssetHolding]
  -- ^ assets held by this account.
  , adAuthAddr :: Maybe Address
  -- ^ the address against which signing should be checked.
  -- If empty, the address of the current account is used. This field can be
  -- updated in any transaction by setting the RekeyTo field.
  , adClosedAtRound :: Maybe Round
  -- ^ Round during which this account was most recently closed.
  , adCreatedAtRound :: Round
  -- ^ Round during which this account first appeared in a transaction.
  , adDeleted :: Bool
  -- ^ Whether or not this account is currently closed.
  , adPendingRewards :: Microalgos
  -- ^ amount of MicroAlgos of pending rewards in this account.
  , adRewardBase :: Maybe Microalgos
  -- ^ used as part of the rewards computation. Only applicable to accounts
  -- which are participating.
  , adRewards :: Microalgos
  -- ^ total rewards of MicroAlgos the account has received, including pending rewards.
  , adRound :: Round
  -- ^ the round for which this information is relevant.
  , adSigType :: SignatureType
  -- ^ indicates what type of signature is used by this account
  , adStatus :: Text
  -- ^ delegation status of the account's MicroAlgos
  } deriving (Generic, Show)
$(deriveJSON algorandTrainOptions 'AccountData)

data IdxAccountResponse = IdxAccountResponse
  { iarAccount :: AccountData
  , iarCurrentRound :: Round
  } deriving stock (Generic, Show)
$(deriveJSON algorandTrainOptions 'IdxAccountResponse)

-- | An Algorand transaction response.
-- Contains all fields common to all transactions and serves as an envelope
-- to all transactions type.
data TransactionResp = TransactionResp
  { tAuthAddr :: Maybe Address
  -- ^ [sgnr] this is included with signed transactions when the signing
  -- address does not equal the sender. The backend can use this to ensure
  -- that auth addr is equal to the accounts auth addr.
  , tCloseRewards :: Maybe Microalgos
  -- ^ [rc] rewards applied to close-remainder-to account.
  , tClosingAmount :: Maybe Microalgos
  -- ^ [ca] closing amount for transaction.
  , tConfirmedRound :: Maybe Round
  -- ^ Round when the transaction was confirmed.
  , tCreatedApplicationIndex :: Maybe Integer
  -- ^ Specifies an application index (ID) if an application was created
  -- with this transaction.
  , tCreatedAssetIndex :: Maybe Integer
  -- ^ Specifies an asset index (ID) if an asset was created with this transaction.
  , tFee :: Microalgos
  -- ^ [fee] Transaction fee.
  , tFirstValid :: Round
  -- ^ [fv] First valid round for this transaction.
  , tGenesisHash :: Maybe GenesisHash
  -- ^ [gh] Hash of genesis block.
  , tGenesisId :: Maybe Text
  -- ^ [gen] genesis block ID.
  , tGroup :: Maybe TransactionGroupId
  -- ^ [grp] Base64 encoded byte array of a sha512/256 digest. When present
  -- indicates that this transaction is part of a transaction group and the
  -- value is the sha512/256 hash of the transactions in that group.
  , tId :: Text
  -- ^ Transaction ID
  , tIntraRoundOffset :: Maybe Round
  -- ^ Offset into the round where this transaction was confirmed.
  , tLastValid :: Round
  -- ^ [lv] Last valid round for this transaction.
  , tLease :: Maybe Lease
  -- ^ [lx] Base64 encoded 32-byte array. Lease enforces mutual exclusion of
  -- transactions.  If this field is nonzero, then once the transaction is
  -- confirmed, it acquires the lease identified by the (Sender, Lease) pair
  -- of the transaction until the LastValid round passes.  While this
  -- transaction possesses the lease, no other transaction specifying this
  -- lease can be confirmed.
  , tNote :: Maybe ByteString
  -- ^ [note] Free form data.
  , tReceiverRewards :: Maybe Microalgos
  -- ^ [rr] rewards applied to receiver account.
  , tRekeyTo :: Maybe Address
  -- ^ [rekey] when included in a valid transaction, the accounts auth addr
  -- will be updated with this value and future signatures must be signed with
  -- the key represented by this address.
  , tSender :: Address
  -- ^ [snd] Sender's address.
  , tSenderRewards:: Maybe Microalgos
  -- ^ [rs] rewards applied to sender account.
  , tSignature :: SignatureType
  -- ^ Validation signature associated with some data. Only one of the
  -- signatures should be provided.
  , tTxType :: TransactionType
  -- ^ [type] Indicates what type of transaction this is.
  -- Different types have different fields.
  } deriving (Eq, Generic, Show)

instance ToJSON TransactionResp where
  toJSON TransactionResp{..} = do
    let txType :: Text = case tTxType of
          PaymentTransaction{} -> "pay"
          ApplicationCallTransaction{} -> "appl"
          AssetTransferTransaction{} -> "axfer"
          KeyRegistrationTransaction{} -> "keyreg"
          AssetConfigTransaction{} -> "acfg"
          AssetFreezeTransaction{} -> "afrz"
    object
      [ "auth-addr" .= tAuthAddr
      , "close-rewards" .= tCloseRewards
      , "closing-amount" .= tClosingAmount
      , "confirmed-round" .= tConfirmedRound
      , "created-application-index" .= tCreatedApplicationIndex
      , "created-asset-index" .= tCreatedAssetIndex
      , "fee" .= tFee
      , "first-valid" .= tFirstValid
      , "genesis-hash" .= tGenesisHash
      , "genesis-id" .= tGenesisId
      , "group" .= tGroup
      , "id" .= tId
      , "intra-round-offset" .= tIntraRoundOffset
      , "last-valid" .= tLastValid
      , "lease" .= tLease
      , "note" .= tNote
      , "receiver-rewards" .= tReceiverRewards
      , "rekey-to" .= tRekeyTo
      , "sender" .= tSender
      , "sender-rewards" .= tSenderRewards
      , "signature" .= tSignature
      , "txType" .= txType
      ]

instance FromJSON TransactionResp where
  parseJSON = withObject "TransactionResp" $ \o -> do
    txType :: Text <- o .: "tx-type" -- something wrong here
    TransactionResp
      <$> o .:? "auth-addr"
      <*> o .:? "close-rewards"
      <*> o .:? "closing-amount"
      <*> o .:? "confirmed-round"
      <*> o .:? "created-application-index"
      <*> o .:? "created-asset-index"
      <*> o .:  "fee"
      <*> o .:  "first-valid"
      <*> o .:? "genesis-hash"
      <*> o .:? "genesis-id"
      <*> o .:? "group"
      <*> o .:  "id"
      <*> o .:? "intra-round-offset"
      <*> o .:  "last-valid"
      <*> o .:? "lease"
      <*> o .:? "note"
      <*> o .:? "receiver-rewards"
      <*> o .:? "rekey-to"
      <*> o .:  "sender"
      <*> o .:? "sender-rewards"
      <*> o .:  "signature"
      <*> case txType of
        "pay" -> o .: "payment-transaction"
        "appl" -> o .: "application-call-transaction"
        "axfer" -> o .: "asset-transfer-transaction"
        "keyreg" -> o .: "key-registration-transaction"
        "acfg" -> o .: "asset-config-transaction"
        "afrz" -> o .: "asset-freeze-transaction"
        x -> fail . T.unpack $ "Unmapped transaction type field name: " <> x

    --       "pay" -> PaymentTransaction
    --         <$> o .: "receiver"
    --         <*> o .: "amount"
    --         <*> o .: "close-amount"


-- | An Algorand Block information.
data BlockResp = BlockResp
  { brGenesisHash :: GenesisHash
  -- ^ [gh] hash to which this block belongs.
  , brGenesisId :: Text
  -- ^ [gen] ID to which this block belongs.
  , brPreviousBlockHash :: BlockHash
  -- ^ [prev] hash of the previous block.
  , brRewards :: Maybe Rewards
  , brRound :: Round
  -- ^ [rnd] Current round on which this block was appended to the chain.
  , brSeed :: Seed
  -- ^ [seed] Sortition seed.
  , brTimestamp :: POSIXTime
  -- ^ [ts] Block creation timestamp in seconds since epoch.
  , brTransactions :: Maybe [TransactionResp]
  -- ^ [txns] list of transactions corresponding
  -- to a given round.
  -- Note: for some reasons indexer doesn't return this field at all instead
  -- of empty list in case when there aro no transactions
  , brTransactionsRoot :: Maybe TransactionsRoot
  -- ^ [txn] TransactionsRoot authenticates the set of transactions appearing
  -- in the block. More specifically, it's the root of a merkle tree whose
  -- leaves are the block's Txids, in lexicographic order.
  -- For the empty block, it's 0. Note that the TxnRoot does not authenticate
  -- the signatures on the transactions, only the transactions themselves.
  -- Two blocks with the same transactions but in a different order and with
  -- different signatures will have the same TxnRoot.
  , brTxnCounter :: Maybe Word64
  -- ^ [tc] TxnCounter counts the number of transactions committed in the
  -- ledger, from the time at which support for this feature was introduced.
  -- Specifically, this counter is the number of the next transaction that will
  -- be committed after this block.
  -- It is 0 when no transactions have ever been committed (since counter
  -- started being supported).
  , brUpgradeState :: Maybe UpgradeState
  , brUpgradeVote :: Maybe UpgradeVote
  } deriving stock (Generic, Show)
$(deriveJSON algorandTrainOptions 'BlockResp)

-- | Algod Indexer API
data IndexerApi route = IndexerApi
  { _accountIdx :: route
      :- "v2"
      :> "accounts"
      :> Capture "address" Address
      :> QueryParam "round" Round
      :> Get '[JSON] IdxAccountResponse
  , _blockIdx :: route
      :- "v2"
      :> "blocks"
      :> Capture "round" Round
      :> Get '[JSON] BlockResp
  } deriving (Generic)
