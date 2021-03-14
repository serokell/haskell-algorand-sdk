-- SPDX-FileCopyrightText: 2021 Serokell <https://serokell.io>
--
-- SPDX-License-Identifier: MPL-2.0

-- | Tools for manipulating Algorand transactions.
module Data.Algorand.Transaction
  ( Transaction (..)
  , TransactionType (..)

  , AppIndex
  , AssetIndex
  , GenesisHash
  , TransactionGroupId
  , Lease
  , StateSchema (..)

  , OnComplete
  , onCompleteNoOp
  , onCompleteOptIn
  , onCompleteCloseOut
  , onCompleteClearState
  , onCompleteUpdateApplication
  , onCompleteDeleteApplication

  , SignedTransaction ()
  , signTransaction
  , verifyTransaction
  , getUnverifiedTransaction

  , transactionId
  , transactionId'
  ) where

import Data.Aeson (FromJSON (..), Options (constructorTagModifier, fieldLabelModifier, sumEncoding), SumEncoding (TaggedObject), ToJSON (..), Value (Object), genericToEncoding, genericToJSON, genericParseJSON)
import Data.Aeson.Types (withObject)
import Data.ByteArray (Bytes)
import Data.ByteArray.Sized (SizedByteArray, unSizedByteArray)
import Data.ByteString (ByteString)
import Data.ByteString.Base32 (encodeBase32Unpadded)
import Data.ByteString.Lazy (toStrict)
import qualified Data.HashMap.Strict as HM
import Data.MessagePack (pack)
import Data.String (IsString)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Word (Word64)
import GHC.Generics (Generic)

import Crypto.Algorand.Hash (hash32)
import Crypto.Algorand.Signature (SecretKey, Signature, sign, verify)
import Data.Algorand.Address (Address, toPublicKey)
import Data.Algorand.Amount (Microalgos)
import Data.Algorand.MessagePack (MessagePackObject (toCanonicalObject), MessageUnpackObject (fromCanonicalObject), Canonical (Canonical), (&), (&<>), (.=), (.=<), (.:), (.:?), (.:>))
import Network.Algorand.Node.Api.Json (defaultOptions)


type AppIndex = Word64
type AssetIndex = Word64

type GenesisHash = SizedByteArray 32 Bytes

type TransactionGroupId = SizedByteArray 32 Bytes

type Lease = SizedByteArray 32 Bytes

-- | An Algorand transaction (only Header fields).
data Transaction = Transaction
  { tSender :: Address
  , tFee :: Microalgos
  , tFirstValid :: Word64
  , tLastValid :: Word64
  , tNote :: Maybe ByteString
  , tGenesisId :: Maybe Text
  , tGenesisHash :: GenesisHash

  , tTxType :: TransactionType
  , tGroup :: Maybe TransactionGroupId
  , tLease :: Maybe Lease
  , tRekeyTo :: Maybe Address

  -- TxType is set automatically
  }
  deriving (Eq, Generic, Show)

-- | Specific types of Algorand transactions.
data TransactionType
  = PaymentTransaction
    { ptReceiver :: Address
    , ptAmount :: Microalgos
    , ptCloseRemainderTo :: Maybe Address
    }
  | ApplicationCallTransaction
    { actApplicationId :: AppIndex
    , actOnComplete :: OnComplete
    , actAccounts :: [Address]
    , actApprovalProgram :: Maybe ByteString
    , actAppArguments :: [ByteString]
    , actClearStateProgram :: Maybe ByteString
    , actForeignApps :: [AppIndex]
    , actForeignAssets :: [AssetIndex]
    , actGlobalStateSchema :: Maybe StateSchema
    , actLocalStateSchema :: Maybe StateSchema
    }
  | AssetTransferTransaction
    { attXferAsset :: AssetIndex
    , attAssetAmount :: Word64
    , attAssetSender :: Maybe Address  -- required in a spec, but not really
    , attAssetReceiver :: Address
    , attAssetCloseTo :: Maybe Address
    }
  -- TODO:
  | KeyRegistrationTransaction
    {}
  | AssetConfigTransaction
    {}
  | AssetFreezeTransaction
    {}
  deriving (Eq, Generic, Show)

-- | Constants for @OnComplete@.
--
type OnComplete = Word64

onCompleteNoOp, onCompleteOptIn, onCompleteCloseOut, onCompleteClearState, onCompleteUpdateApplication, onCompleteDeleteApplication :: OnComplete
onCompleteNoOp = 0
onCompleteOptIn = 1
onCompleteCloseOut = 2
onCompleteClearState = 3
onCompleteUpdateApplication = 4
onCompleteDeleteApplication = 5


-- | The 'StateSchema' object.
data StateSchema = StateSchema
  { ssNumUint :: Word64
  , ssNumByteSlice :: Word64
  }
  deriving (Eq, Generic, Show)


-- | A signed transaction object.
data SignedTransaction = SignedTransaction
  -- TODO: Only simple signature is supported for now.
  { stSig :: Signature
  , stTxn :: Transaction
  }
  deriving (Generic, Show)

serialiseTx :: Transaction -> ByteString
serialiseTx = toStrict . ("TX" <>) . pack . Canonical

-- | Sign a transaction.
signTransaction :: SecretKey -> Transaction -> SignedTransaction
signTransaction sk txn = SignedTransaction
  { stTxn = txn
  , stSig = sign sk (serialiseTx txn)
  }

-- | Verify a signed transaction.
verifyTransaction :: SignedTransaction -> Maybe Transaction
verifyTransaction SignedTransaction{..} =
  let pk = toPublicKey (tSender stTxn) in
  case verify pk (serialiseTx stTxn) stSig of
    False -> Nothing
    True -> Just stTxn

-- | Dangerous: returns a transaction without verifying the signature.
getUnverifiedTransaction :: SignedTransaction -> Transaction
getUnverifiedTransaction = stTxn


-- | Get transaction ID.
transactionId :: Transaction -> Text
transactionId = encodeBase32Unpadded . transactionId'

-- | Get transaction ID as raw bytes.
transactionId' :: Transaction -> ByteString
transactionId' = unSizedByteArray . hash32 . serialiseTx

{-
 - What comes below is pretty annoying. Basically, it is a ton of boilerplate
 - that converts the data types above to “canonical” MessagePack.
 -
 - It might be possible to automate this using Generics, however doing this
 - manually for now will probably be much faster.
 -}

transactionFieldName :: IsString s => String -> s
transactionFieldName = \case
  "tSender" -> "snd"
  "tFee" -> "fee"
  "tFirstValid" -> "fv"
  "tLastValid" -> "lv"
  "tNote" -> "note"
  "tGenesisId" -> "gen"
  "tGenesisHash" -> "gh"
  "tGroup" -> "grp"
  "tLease" -> "lx"
  "tRekeyTo" -> "rekey"
  "tTxType" -> "type"
  x -> error $ "Unmapped transaction field name: " <> x

instance MessagePackObject Transaction where
  toCanonicalObject Transaction{..} = mempty
      & f "tSender" .= tSender
      & f "tFee" .= tFee
      & f "tFirstValid" .= tFirstValid
      & f "tLastValid" .= tLastValid
      & f "tNote" .= tNote
      & f "tGenesisId" .= tGenesisId
      & f "tGenesisHash" .= tGenesisHash
      & f "tGroup" .= tGroup
      & f "tLease" .= tLease
      & f "tRekeyTo" .= tRekeyTo
      &<> tTxType
    where
      f = transactionFieldName

instance MessageUnpackObject Transaction where
  fromCanonicalObject o = do
      tSender <- o .:? f "tSender"
      tFee <- o .:? f "tFee"
      tFirstValid <- o .:? f "tFirstValid"
      tLastValid <- o .:? f "tLastValid"
      tNote <- o .:? f "tNote"
      tGenesisId <- o .:? f "tGenesisId"
      tGenesisHash <- o .: f "tGenesisHash"
      tGroup <- o .:? f "tGroup"
      tLease <- o .:? f "tLease"
      tRekeyTo <- o .:? f "tRekeyTo"
      tTxType <- fromCanonicalObject o
      pure Transaction{..}
    where
      f = transactionFieldName

transactionJsonOptions :: Options
transactionJsonOptions = defaultOptions { fieldLabelModifier = transactionFieldName }

-- These instances do surgery on @Value@s in order to inline TransactionType
-- into its surrounding Transaction.
-- It is a bit crazy, but hopefully OK, since we have round-trip tests...
instance ToJSON Transaction where
  toJSON txn = case toJSON' txn of
      Object hm -> case HM.lookup "type" hm of
        Just (Object hm') -> Object $ hm' <> hm  -- union taking "type" form the internal one
        _ -> error "Incorrect encoding of TransactionType"
      _ -> error "Incorrect encoding of Transaction"
    where
      toJSON' = genericToJSON transactionJsonOptions

instance FromJSON Transaction where
  parseJSON o = do
      txnType <- parseJSON @TransactionType o
      withObject "Transaction" (parseJSON' . Object . HM.insert "type" (toJSON txnType)) o
    where
      parseJSON' = genericParseJSON transactionJsonOptions


transactionTypeFieldName :: IsString s => String -> s
transactionTypeFieldName = \case
  "ptReceiver" -> "rcv"
  "ptAmount" -> "amt"
  "ptCloseRemainderTo" -> "close"

  "actApplicationId" -> "apid"
  "actOnComplete" -> "apan"
  "actAccounts" -> "apat"
  "actApprovalProgram" -> "apap"
  "actAppArguments" -> "apaa"
  "actClearStateProgram" -> "apsu"
  "actForeignApps" -> "apfa"
  "actForeignAssets" -> "apas"
  "actGlobalStateSchema" -> "apgs"
  "actLocalStateSchema" -> "apls"

  "attXferAsset" -> "xaid"
  "attAssetAmount" -> "aamt"
  "attAssetSender" -> "asnd"
  "attAssetReceiver" -> "arcv"
  "attAssetCloseTo" -> "aclose"

  x -> error $ "Unmapped transaction type field name: " <> x

transactionType :: IsString s => String -> s
transactionType = \case
  "PaymentTransaction" -> "pay"
  "ApplicationCallTransaction" -> "appl"
  "KeyRegistrationTransaction" -> "keyreg"
  "AssetConfigTransaction" -> "acfg"
  "AssetTransferTransaction" -> "axfer"
  "AssetFreezeTransaction" -> "afrz"
  x -> error $ "Unmapped transaction type constructor: " <> x

instance MessagePackObject TransactionType where
  toCanonicalObject  = \case
      PaymentTransaction{..} -> mempty
        & "type" .= t "PaymentTransaction"
        & f "ptReceiver" .= ptReceiver
        & f "ptAmount" .= ptAmount
        & f "ptCloseRemainderTo" .= ptCloseRemainderTo
      ApplicationCallTransaction{..} -> mempty
        & "type" .= t "ApplicationCallTransaction"
        & f "actApplicationId" .= actApplicationId
        & f "actOnComplete" .= actOnComplete
        & f "actAccounts" .= actAccounts
        & f "actApprovalProgram" .= actApprovalProgram
        & f "actAppArguments" .= actAppArguments
        & f "actClearStateProgram" .= actClearStateProgram
        & f "actForeignApps" .= actForeignApps
        & f "actForeignAssets" .= actForeignAssets
        & f "actGlobalStateSchema" .=< actGlobalStateSchema
        & f "actLocalStateSchema" .=< actLocalStateSchema
      KeyRegistrationTransaction -> mempty
        & "type" .= t "KeyRegistrationTransaction"
      AssetConfigTransaction -> mempty
        & "type" .= t "AssetConfigTransaction"
      AssetTransferTransaction{..} -> mempty
        & "type" .= t "AssetTransferTransaction"
        & f "attXferAsset" .= attXferAsset
        & f "attAssetAmount" .= attAssetAmount
        & f "attAssetSender" .= attAssetSender
        & f "attAssetReceiver" .= attAssetReceiver
        & f "attAssetCloseTo" .= attAssetCloseTo
      AssetFreezeTransaction -> mempty
        & "type" .= t "AssetFreezeTransaction"
    where
      f = transactionTypeFieldName
      t = transactionType :: String -> Text

instance MessageUnpackObject TransactionType where
  fromCanonicalObject o = o .: "type" >>= \case
      "pay" -> do
        ptReceiver <- o .:? f "ptReceiver"
        ptAmount <- o .:? f "ptAmount"
        ptCloseRemainderTo <- o .:? f "ptCloseRemainderTo"
        pure PaymentTransaction{..}
      "appl" -> do
        actApplicationId <- o .:? f "actApplicationId"
        actOnComplete <- o .:? f "actOnComplete"
        actAccounts <- o .:? f "actAccounts"
        actApprovalProgram <- o .:? f "actApprovalProgram"
        actAppArguments <- o .:? f "actAppArguments"
        actClearStateProgram <- o .:? f "actClearStateProgram"
        actForeignApps <- o .:? f "actForeignApps"
        actForeignAssets <- o .:? f "actForeignAssets"
        actGlobalStateSchema <- o .:> f "actGlobalStateSchema"
        actLocalStateSchema <- o .:> f "actLocalStateSchema"
        pure ApplicationCallTransaction{..}
      "keyreg" -> do
        pure KeyRegistrationTransaction
      "acfg" -> do
        pure AssetConfigTransaction
      "axfer" -> do
        attXferAsset <- o .:? f "attXferAsset"
        attAssetAmount <- o .:? f "attAssetAmount"
        attAssetSender <- o .:? f "attAssetSender"
        attAssetReceiver <- o .:? f "attAssetReceiver"
        attAssetCloseTo <- o .:? f "attAssetCloseTo"
        pure AssetTransferTransaction{..}
      "afrz" -> do
        pure AssetFreezeTransaction
      x -> fail $ "Unsupported transaction type: " <> T.unpack x
    where
      f = transactionTypeFieldName

transactionTypeJsonOptions :: Options
transactionTypeJsonOptions = defaultOptions
  { fieldLabelModifier = transactionTypeFieldName
  , constructorTagModifier = transactionType
  , sumEncoding = TaggedObject "type" "_"
  }

instance ToJSON TransactionType where
  toJSON = genericToJSON transactionTypeJsonOptions
  toEncoding = genericToEncoding transactionTypeJsonOptions

instance FromJSON TransactionType where
  parseJSON = genericParseJSON transactionTypeJsonOptions


stateSchemaFieldName :: IsString s => String -> s
stateSchemaFieldName = \case
  "ssNumUint" -> "nui"
  "ssNumByteSlice" -> "nbs"
  x -> error $ "Unmapped state schema field name: " <> x

instance MessagePackObject StateSchema where
  toCanonicalObject StateSchema{..} = mempty
      & f "ssNumUint" .= ssNumUint
      & f "ssNumByteSlice" .= ssNumByteSlice
    where
      f = stateSchemaFieldName

instance MessageUnpackObject StateSchema where
  fromCanonicalObject o = do
      ssNumUint <- o .:? f "ssNumUint"
      ssNumByteSlice <- o .:? f "ssNumByteSlice"
      pure StateSchema{..}
    where
      f = stateSchemaFieldName

stateSchemaJsonOptions :: Options
stateSchemaJsonOptions = defaultOptions { fieldLabelModifier = stateSchemaFieldName }

instance ToJSON StateSchema where
  toJSON = genericToJSON stateSchemaJsonOptions
  toEncoding = genericToEncoding stateSchemaJsonOptions

instance FromJSON StateSchema where
  parseJSON = genericParseJSON stateSchemaJsonOptions


signedTransactionFieldName :: IsString s => String -> s
signedTransactionFieldName = \case
  "stSig" -> "sig"
  "stTxn" -> "txn"
  x -> error $ "Unmapped signed transaction field name: " <> x

instance MessagePackObject SignedTransaction where
  toCanonicalObject SignedTransaction{..} = mempty
      & f "stSig" .= stSig
      & f "stTxn" .=< stTxn
    where
      f = signedTransactionFieldName

instance MessageUnpackObject SignedTransaction where
  fromCanonicalObject o = do
      stSig <- o .: f "stSig"
      stTxn <- o .:> f "stTxn"
      pure SignedTransaction{..}
    where
      f = signedTransactionFieldName

signedTransactionJsonOptions :: Options
signedTransactionJsonOptions = defaultOptions { fieldLabelModifier = signedTransactionFieldName }

instance ToJSON SignedTransaction where
  toJSON = genericToJSON signedTransactionJsonOptions
  toEncoding = genericToEncoding signedTransactionJsonOptions

instance FromJSON SignedTransaction where
  parseJSON = genericParseJSON signedTransactionJsonOptions
