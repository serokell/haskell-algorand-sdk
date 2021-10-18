-- SPDX-FileCopyrightText: 2021 Serokell <https://serokell.io>
--
-- SPDX-License-Identifier: MPL-2.0

-- | The REST API v2 of the Algorand indexer.
--
-- See <https://developer.algorand.org/docs/reference/rest-apis/indexer/>
module Network.Algorand.Api.Indexer
  ( Health (..)
  , IndexerApi (..)
  , IdxAccountResponse (..)
  , TransactionResp (..)
  , transactionRespToTransaction
  , BlockResp (..)
  ) where

import qualified Data.Text as T

import Data.Aeson (FromJSON (..), KeyValue ((.=)), ToJSON (..), object, withObject, (.:), (.:?))
import Data.Aeson.TH (deriveJSON)
import Data.Aeson.Types (Object, Pair, Parser)
import Data.ByteString (ByteString)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Time.Clock.POSIX (POSIXTime)
import Data.Word (Word64)
import GHC.Generics (Generic)
import Servant.API (Capture, Get, JSON, QueryParam, (:>))
import Servant.API.Generic ((:-))

import Crypto.Algorand.Signature (SignatureType (..))
import Data.Algorand.Address (Address)
import Data.Algorand.Amount (Microalgos)
import Data.Algorand.Block (BlockHash, Seed, TransactionsRoot)
import Data.Algorand.Round (Round)
import Data.Algorand.Transaction (GenesisHash, Lease, Transaction (..), TransactionGroupId,
                                  TransactionType (..))
import Network.Algorand.Api.Json (algorandTrainOptions)
import Network.Algorand.Api.Node (Account)
import Network.Algorand.Definitions (Network)

----------------
-- Types
----------------

-- | Data sent in response to @/accounts@.
data IdxAccountResponse = IdxAccountResponse
  { iarAccount :: Account
  , iarCurrentRound :: Round
  } deriving stock (Generic, Show)

-- | An Algorand transaction response.
-- Contains all fields common to all transactions and serves as an envelope
-- to all transactions type.
data TransactionResp = TransactionResp
  { trAuthAddr :: Maybe Address
  -- ^ [sgnr] this is included with signed transactions when the signing
  -- address does not equal the sender. The backend can use this to ensure
  -- that auth addr is equal to the accounts auth addr.
  , trCloseRewards :: Maybe Microalgos
  -- ^ [rc] rewards applied to close-remainder-to account.
  , trClosingAmount :: Maybe Microalgos
  -- ^ [ca] closing amount for transaction.
  , trConfirmedRound :: Maybe Round
  -- ^ Round when the transaction was confirmed.
  , trCreatedApplicationIndex :: Maybe Integer
  -- ^ Specifies an application index (ID) if an application was created
  -- with this transaction.
  , trCreatedAssetIndex :: Maybe Integer
  -- ^ Specifies an asset index (ID) if an asset was created with this transaction.
  , trFee :: Microalgos
  -- ^ [fee] Transaction fee.
  , trFirstValid :: Round
  -- ^ [fv] First valid round for this transaction.
  , trGroup :: Maybe TransactionGroupId
  -- ^ [grp] Base64 encoded byte array of a sha512/256 digest. When present
  -- indicates that this transaction is part of a transaction group and the
  -- value is the sha512/256 hash of the transactions in that group.
  , trId :: Text
  -- ^ Transaction ID
  , trIntraRoundOffset :: Maybe Round
  -- ^ Offset into the round where this transaction was confirmed.
  , trLastValid :: Round
  -- ^ [lv] Last valid round for this transaction.
  , trLease :: Maybe Lease
  -- ^ [lx] Base64 encoded 32-byte array. Lease enforces mutual exclusion of
  -- transactions.  If this field is nonzero, then once the transaction is
  -- confirmed, it acquires the lease identified by the (Sender, Lease) pair
  -- of the transaction until the LastValid round passes.  While this
  -- transaction possesses the lease, no other transaction specifying this
  -- lease can be confirmed.
  , trNote :: Maybe ByteString
  -- ^ [note] Free form data.
  , trReceiverRewards :: Maybe Microalgos
  -- ^ [rr] rewards applied to receiver account.
  , trRekeyTo :: Maybe Address
  -- ^ [rekey] when included in a valid transaction, the accounts auth addr
  -- will be updated with this value and future signatures must be signed with
  -- the key represented by this address.
  , trSender :: Address
  -- ^ [snd] Sender's address.
  , trSenderRewards:: Maybe Microalgos
  -- ^ [rs] rewards applied to sender account.
  , trSignature :: SignatureType
  -- ^ Validation signature associated with some data. Only one of the
  -- signatures should be provided.
  , trTxType :: TransactionType
  -- ^ [type] Indicates what type of transaction this is.
  -- Different types have different fields.
  } deriving stock (Eq, Generic, Show)

instance ToJSON TransactionResp where
  toJSON TransactionResp{..} = do
    let (typeTag, typePayload) = transactionTypeToTagAndObject trTxType
    object $ typePayload <>
      [ "auth-addr" .= trAuthAddr
      , "close-rewards" .= trCloseRewards
      , "closing-amount" .= trClosingAmount
      , "confirmed-round" .= trConfirmedRound
      , "created-application-index" .= trCreatedApplicationIndex
      , "created-asset-index" .= trCreatedAssetIndex
      , "fee" .= trFee
      , "first-valid" .= trFirstValid
      , "group" .= trGroup
      , "id" .= trId
      , "intra-round-offset" .= trIntraRoundOffset
      , "last-valid" .= trLastValid
      , "lease" .= trLease
      , "note" .= trNote
      , "receiver-rewards" .= trReceiverRewards
      , "rekey-to" .= trRekeyTo
      , "sender" .= trSender
      , "sender-rewards" .= trSenderRewards
      , "signature" .= trSignature
      , "tx-type" .= typeTag
      ]
    where
      transactionTypeToTagAndObject :: TransactionType -> (Text, [Pair])
      transactionTypeToTagAndObject = \case
        PaymentTransaction {..} ->
          ("pay", [("payment-transaction", object
            [ "receiver" .= ptReceiver
            , "amount" .= ptAmount
            , "close-remainder-to" .= ptCloseRemainderTo
            ]
          )])
        ApplicationCallTransaction {..} ->
          ("appl", [("application-transaction", object
            [ "application-id" .= actApplicationId
            , "on-completion" .= actOnComplete
            , "accounts" .= actAccounts
            , "approval-program" .= actApprovalProgram
            , "application-args" .= actAppArguments
            , "clear-state-program" .= actClearStateProgram
            , "foreign-apps" .= actForeignApps
            , "foreign-assets" .= actForeignAssets
            , "global-state-schema" .= actGlobalStateSchema
            , "local-state-schema" .= actLocalStateSchema
            ]
          )])
        AssetTransferTransaction {..} ->
          ("axfer", [("asset-transfer-transaction", object
            [ "asset-id" .= attXferAsset
            , "amount" .= attAssetAmount
            , "sender" .= attAssetSender
            , "receiver" .= attAssetReceiver
            , "close-to" .= attAssetCloseTo
            ]
          )])
        KeyRegistrationTransaction {} -> ("keyreg", mempty)
        AssetConfigTransaction {} -> ("acfg", mempty)
        AssetFreezeTransaction {} -> ("afrz", mempty)

instance FromJSON TransactionResp where
  parseJSON = withObject "TransactionResp" $ \o -> do
    trAuthAddr <- o .:? "auth-addr"
    trCloseRewards <- o .:? "close-rewards"
    trClosingAmount <- o .:? "closing-amount"
    trConfirmedRound <- o .:? "confirmed-round"
    trCreatedApplicationIndex <- o .:? "created-application-index"
    trCreatedAssetIndex <- o .:? "created-asset-index"
    trFee <- o .: "fee"
    trFirstValid <- o .: "first-valid"
    trGroup <- o .:? "group"
    trId <- o .: "id"
    trIntraRoundOffset <- o .:? "intra-round-offset"
    trLastValid <- o .: "last-valid"
    trLease <- o .:? "lease"
    trNote <- o .:? "note"
    trReceiverRewards <- o .:? "receiver-rewards"
    trRekeyTo <- o .:? "rekey-to"
    trSender <- o .: "sender"
    trSenderRewards <- o .:? "sender-rewards"
    trSignature <- o .: "signature"
    trTxType <- parseTransactionType o =<< o .: "tx-type"
    return TransactionResp {..}
    where
      parseTransactionType :: Object -> Text -> Parser TransactionType
      parseTransactionType o = \case
        "pay" ->
          parsePaymentTransaction =<< o .: "payment-transaction"
        "appl" ->
          parseApplicationCallTransaction =<< o .: "application-transaction"
        "axfer" ->
          parseAssetTransferTransaction =<< o .: "asset-transfer-transaction"
        "keyreg" -> pure KeyRegistrationTransaction
        "acfg" -> pure AssetConfigTransaction
        "afrz" -> pure AssetFreezeTransaction
        x -> fail . T.unpack $ "Unmapped transaction type field name: " <> x

      parsePaymentTransaction subObj = do
        ptReceiver <- subObj .: "receiver"
        ptAmount <- subObj .: "amount"
        ptCloseRemainderTo <- subObj .:? "close-remainder-to"
        return PaymentTransaction {..}

      parseApplicationCallTransaction subObj = do
        actApplicationId <- subObj .: "application-id"
        actOnComplete <- subObj .: "on-completion"
        actAccounts <- fromMaybe [] <$> subObj .:? "accounts"
        actApprovalProgram <- subObj .:? "approval-program"
        actAppArguments <- fromMaybe [] <$> subObj .:? "application-args"
        actClearStateProgram <- subObj .:? "clear-state-program"
        actForeignApps <- fromMaybe [] <$> subObj .:? "foreign-apps"
        actForeignAssets <- fromMaybe [] <$> subObj .:? "foreign-assets"
        actGlobalStateSchema <- subObj .:? "global-state-schema"
        actLocalStateSchema <- subObj .:? "local-state-schema"
        return ApplicationCallTransaction {..}

      parseAssetTransferTransaction subObj = do
        attXferAsset <- subObj .: "asset-id"
        attAssetAmount <- subObj .: "amount"
        attAssetSender <- subObj .:? "sender"
        attAssetReceiver <- subObj .: "receiver"
        attAssetCloseTo <- subObj .:? "close-to"
        return AssetTransferTransaction {..}

transactionRespToTransaction :: Text -> GenesisHash -> TransactionResp -> Transaction
transactionRespToTransaction gId gHash TransactionResp {..} = Transaction
  { tSender = trSender
  , tFee = trFee
  , tFirstValid = trFirstValid
  , tLastValid = trLastValid
  , tNote = trNote
  , tGenesisId = Just gId
  , tGenesisHash = Just gHash
  , tTxType = trTxType
  , tGroup = trGroup
  , tLease = trLease
  , tRekeyTo = trRekeyTo
  }

-- | An Algorand Block as returned by the indexer.
data BlockResp = BlockResp
  { brGenesisHash :: GenesisHash
  -- ^ [gh] hash to which this block belongs.
  , brGenesisId :: Text
  -- ^ [gen] ID to which this block belongs.
  , brPreviousBlockHash :: BlockHash
  -- ^ [prev] hash of the previous block.
  , brRound :: Round
  -- ^ [rnd] Current round on which this block was appended to the chain.
  , brSeed :: Seed
  -- ^ [seed] Sortition seed.
  , brTimestamp :: POSIXTime
  -- ^ [ts] Block creation timestamp in seconds since epoch.
  , brTransactions :: [TransactionResp]
  -- ^ [txns] list of transactions corresponding to a given round.
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
  } deriving stock (Generic, Show)

$(deriveJSON algorandTrainOptions 'IdxAccountResponse)
$(deriveJSON algorandTrainOptions 'BlockResp)

-- | Indexer health information.
-- From: https://indexer.testnet.algoexplorerapi.io/health
data Health = Health
  { hDbAvailable :: Bool
  , hGenesisHash :: Text
  , hGenesisId :: Network
  , hIsMigrating :: Bool
  , hMessage :: Text
  , hRound :: Round
  , hVersion :: Text
  } deriving (Generic, Show)
$(deriveJSON algorandTrainOptions 'Health)

----------------
-- API
----------------

-- | Indexer API.
data IndexerApi route = IndexerApi
  { _health :: route
      :- "health"
      :> Get '[JSON] Health
  , _accountIdx :: route
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
  } deriving stock (Generic)
