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

  , OnComplete (..)

  , transactionId
  , transactionId'

  , serialiseTx
  , encodeUintAppArgument
  , decodeUintAppArgument
  ) where

import qualified Data.Text as T

import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.Binary (decodeOrFail, encode)
import Data.ByteArray (Bytes)
import Data.ByteArray.Sized (SizedByteArray, unSizedByteArray)
import Data.ByteString (ByteString)
import Data.ByteString.Base32 (encodeBase32Unpadded)
import Data.ByteString.Lazy (fromStrict, toStrict)
import Data.Default.Class (Default (def))
import Data.MessagePack (pack)
import Data.String (IsString)
import Data.Text (Text)
import Data.Word (Word64, Word8)
import GHC.Generics (Generic)
import Text.Read (readMaybe)

import Crypto.Algorand.Hash (hash32)
import Data.Algorand.Address (Address)
import Data.Algorand.Amount (Microalgos)
import Data.Algorand.MessagePack (AlgoMessagePack (..), Canonical (Canonical), CanonicalZero,
                                  MessagePackObject (toCanonicalObject),
                                  MessageUnpackObject (fromCanonicalObject), NonZeroValue, (&),
                                  (&<>), (.:), (.:>), (.:?), (.=), (.=<))
import Data.Algorand.MessagePack.Json (parseCanonicalJson, toCanonicalJson)
import Data.Algorand.Round (Round)


type AppIndex = Word64
type AssetIndex = Word64

type GenesisHash = SizedByteArray 32 Bytes

type TransactionGroupId = SizedByteArray 32 Bytes

type Lease = SizedByteArray 32 Bytes

-- | An Algorand transaction (only Header fields).
data Transaction = Transaction
  { tSender :: Address
  , tFee :: Microalgos
  , tFirstValid :: Round
  , tLastValid :: Round
  , tNote :: Maybe ByteString
  , tGenesisId :: Maybe Text
  , tGenesisHash :: Maybe GenesisHash
  , tTxType :: TransactionType
  , tGroup :: Maybe TransactionGroupId
  , tLease :: Maybe Lease
  , tRekeyTo :: Maybe Address

  -- TxType is set automatically
  } deriving (Eq, Generic, Show)

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
data OnComplete
  = OnCompleteNoOp
  | OnCompleteOptIn
  | OnCompleteCloseOut
  | OnCompleteClearState
  | OnCompleteUpdateApplication
  | OnCompleteDeleteApplication
  deriving (Bounded, Eq, Enum)

instance Default OnComplete where
  def = OnCompleteNoOp
instance CanonicalZero OnComplete
instance NonZeroValue OnComplete

instance Show OnComplete where
  show OnCompleteNoOp = "noop"
  show OnCompleteOptIn = "optin"
  show OnCompleteCloseOut = "closeout"
  show OnCompleteClearState = "clear"
  show OnCompleteUpdateApplication = "update"
  show OnCompleteDeleteApplication = "delete"

instance Read OnComplete where
  readsPrec _ = \case
    "noop" -> [(OnCompleteNoOp, "")]
    "optin" -> [(OnCompleteOptIn, "")]
    "closeout" -> [(OnCompleteCloseOut, "")]
    "clear" -> [(OnCompleteClearState, "")]
    "update" -> [(OnCompleteUpdateApplication, "")]
    "delete" -> [(OnCompleteDeleteApplication, "")]
    _ -> []

instance AlgoMessagePack OnComplete where
  toAlgoObject = toAlgoObject @Word8 . fromIntegral . fromEnum
  fromAlgoObject o = (toEnum . fromIntegral) <$> fromAlgoObject @Word8 o

instance ToJSON OnComplete where
  toJSON = toJSON . show

instance FromJSON OnComplete where
  parseJSON o = parseJSON @String o
    >>= maybe (fail "Fail to parse `OnComplete`") pure . readMaybe


-- | The 'StateSchema' object.
data StateSchema = StateSchema
  { ssNumUint :: Word64
  , ssNumByteSlice :: Word64
  } deriving (Eq, Generic, Show)


-- | Get transaction ID.
transactionId :: Transaction -> Text
transactionId = encodeBase32Unpadded . transactionId'

-- | Get transaction ID as raw bytes.
transactionId' :: Transaction -> ByteString
transactionId' = unSizedByteArray . hash32 . serialiseTx

-- | Internal: pack a transaction into byte with prefix.
serialiseTx :: Transaction -> ByteString
serialiseTx = toStrict . ("TX" <>) . pack . Canonical


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
      tGenesisHash <- o .:? f "tGenesisHash"
      tGroup <- o .:? f "tGroup"
      tLease <- o .:? f "tLease"
      tRekeyTo <- o .:? f "tRekeyTo"
      tTxType <- fromCanonicalObject o
      pure Transaction{..}
    where
      f = transactionFieldName

instance ToJSON Transaction where
  toJSON = toCanonicalJson

instance FromJSON Transaction where
  parseJSON = parseCanonicalJson

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

instance ToJSON TransactionType where
  toJSON = toCanonicalJson

instance FromJSON TransactionType where
  parseJSON = parseCanonicalJson

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

instance ToJSON StateSchema where
  toJSON = toCanonicalJson

instance FromJSON StateSchema where
  parseJSON = parseCanonicalJson

-- | Convert integer contract arguments (which passed as ByteString) to uint
decodeUintAppArgument :: ByteString -> Maybe Word64
decodeUintAppArgument = either (const Nothing) (pure . i3)
  . decodeOrFail
  . fromStrict
  where
    i3 (_, _, a) = a

-- | Convert uint to ByteString
-- (this is reversed version of 'decodeUintAppArgument')
encodeUintAppArgument :: Word64 -> ByteString
encodeUintAppArgument = toStrict . encode
