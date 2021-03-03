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
  , OnComplete (..)
  , StateSchema (..)

  , SignedTransaction ()
  , signTransaction
  , verifyTransaction
  , getUnverifiedTransaction

  , transactionId
  ) where

import Data.ByteArray (Bytes)
import Data.ByteArray.Sized (SizedByteArray, unSizedByteArray)
import Data.ByteString (ByteString)
import Data.ByteString.Base32 (encodeBase32Unpadded)
import Data.ByteString.Lazy (toStrict)
import Data.Default.Class (Default (def))
import Data.MessagePack (MessagePack (fromObject, toObject), pack)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Word (Word64)
import GHC.Generics (Generic)

import Crypto.Algorand.Hash (hash32)
import Crypto.Algorand.Signature (SecretKey, Signature, sign, verify)
import Data.Algorand.Address (Address, toPublicKey)
import Data.Algorand.Amount (Microalgos)
import Data.Algorand.MessagePack (AlgorandMessagePack (toCanonicalObject), AlgorandMessageUnpack (fromCanonicalObject), Canonical (Canonical), (&), (&<>), (.=), (.=<), (.:), (.:?), (.:>), NonZeroValue, CanonicalZero)


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
  deriving (Eq, Generic, Show)

-- | Constants for @OnComplete@.
data OnComplete
  = OnCompleteNoOp
  | OnCompleteOptIn
  | OnCompleteCloseOut
  | OnCompleteClearState
  | OnCompleteUpdateApplication
  | OnCompleteDeleteApplication
  | OnCompleteUnknown !Word64  -- ^ For future compatibility
  deriving (Eq, Show)

instance Default OnComplete where
  def = OnCompleteNoOp
instance CanonicalZero OnComplete
instance NonZeroValue OnComplete

instance Enum OnComplete where
  fromEnum OnCompleteNoOp = 0
  fromEnum OnCompleteOptIn = 1
  fromEnum OnCompleteCloseOut = 2
  fromEnum OnCompleteClearState = 3
  fromEnum OnCompleteUpdateApplication = 4
  fromEnum OnCompleteDeleteApplication = 5
  fromEnum (OnCompleteUnknown x) = fromIntegral x  -- XXX: This can overflow

  toEnum 0 = OnCompleteNoOp
  toEnum 1 = OnCompleteOptIn
  toEnum 2 = OnCompleteCloseOut
  toEnum 3 = OnCompleteClearState
  toEnum 4 = OnCompleteUpdateApplication
  toEnum 5 = OnCompleteDeleteApplication
  toEnum x
    | x < 0 = error "toEnum: Negative OnComplete value"
    | otherwise = OnCompleteUnknown (fromIntegral x)

instance Bounded OnComplete where
  minBound = OnCompleteNoOp
  maxBound = OnCompleteDeleteApplication

instance MessagePack OnComplete where
  toObject = toObject @Word64 . fromIntegral . fromEnum
  fromObject o = (toEnum . fromIntegral) <$> fromObject @Word64 o

-- | The 'StateSchema' object.
data StateSchema = StateSchema
  { ssNumberInts :: Word64
  , ssNumberByteSlices :: Word64
  }
  deriving (Eq, Generic, Show)


-- | A signed transaction object.
data SignedTransaction = SignedTransaction
  -- TODO: Only simple signature is supported for now.
  { stSig :: Signature
  , stTransaction :: Transaction
  }
  deriving (Generic, Show)

serialiseTx :: Transaction -> ByteString
serialiseTx = toStrict . ("TX" <>) . pack . Canonical

-- | Sign a transaction.
signTransaction :: SecretKey -> Transaction -> SignedTransaction
signTransaction sk tx = SignedTransaction
  { stTransaction = tx
  , stSig = sign sk (serialiseTx tx)
  }

-- | Verify a signed transaction.
verifyTransaction :: SignedTransaction -> Maybe Transaction
verifyTransaction SignedTransaction{..} =
  let pk = toPublicKey (tSender stTransaction) in
  case verify pk (serialiseTx stTransaction) stSig of
    False -> Nothing
    True -> Just stTransaction

-- | Dangerous: returns a transaction without verifying the signature.
getUnverifiedTransaction :: SignedTransaction -> Transaction
getUnverifiedTransaction = stTransaction


transactionId :: Transaction -> Text
transactionId = encodeBase32Unpadded . unSizedByteArray . hash32 . serialiseTx

{-
 - What comes below is pretty annoying. Basically, it is a ton of boilerplate
 - that converts the data types above to “canonical” MessagePack.
 -
 - It might be possible to automate this using Generics, however doing this
 - manually for now will probably be much faster.
 -}


instance AlgorandMessagePack Transaction where
  toCanonicalObject Transaction{..} = mempty
    & "snd" .= tSender
    & "fee" .= tFee
    & "fv" .= tFirstValid
    & "lv" .= tLastValid
    & "note" .= tNote
    & "gen" .= tGenesisId
    & "gh" .= tGenesisHash
    & "grp" .= tGroup
    & "lx" .= tLease
    & "rekey" .= tRekeyTo
    &<> tTxType

instance AlgorandMessageUnpack Transaction where
  fromCanonicalObject o = do
    tSender <- o .: "snd"
    tFee <- o .:? "fee"
    tFirstValid <- o .:? "fv"
    tLastValid <- o .:? "lv"
    tNote <- o .:? "note"
    tGenesisId <- o .:? "gen"
    tGenesisHash <- o .: "gh"
    tGroup <- o .:? "grp"
    tLease <- o .:? "lx"
    tRekeyTo <- o .:? "rekey"
    tTxType <- fromCanonicalObject o
    pure Transaction{..}


instance AlgorandMessagePack TransactionType where
  toCanonicalObject PaymentTransaction{..} = mempty
    & "type" .= T.pack "pay"
    & "rcv" .= ptReceiver
    & "amt" .= ptAmount
    & "close" .= ptCloseRemainderTo
  toCanonicalObject ApplicationCallTransaction{..} = mempty
    & "type" .= T.pack "appl"
    & "apid" .= actApplicationId
    & "apan" .= actOnComplete
    & "apat" .= actAccounts
    & "apap" .= actApprovalProgram
    & "apaa" .= actAppArguments
    & "apsu" .= actClearStateProgram
    & "apfa" .= actForeignApps
    & "apas" .= actForeignAssets
    & "apgs" .=< actGlobalStateSchema
    & "apls" .=< actLocalStateSchema

instance AlgorandMessageUnpack TransactionType where
  fromCanonicalObject o = o .: "type" >>= \case
    "pay" -> do
      ptReceiver <- o .: "rcv"
      ptAmount <- o .:? "amt"
      ptCloseRemainderTo <- o .:? "close"
      pure PaymentTransaction{..}
    "appl" -> do
      actApplicationId <- o .:? "apid"
      actOnComplete <- o .:? "apan"
      actAccounts <- o .:? "apat"
      actApprovalProgram <- o .:? "apap"
      actAppArguments <- o .:? "apaa"
      actClearStateProgram <- o .:? "apsu"
      actForeignApps <- o .:? "apfa"
      actForeignAssets <- o .:? "apas"
      actGlobalStateSchema <- o .:> "apgs"
      actLocalStateSchema <- o .:> "apls"
      pure ApplicationCallTransaction{..}
    x -> fail $ "Unsupported transaction type: " <> x


instance AlgorandMessagePack StateSchema where
  toCanonicalObject StateSchema{..} = mempty
    & "nui" .= ssNumberInts
    & "nbs" .= ssNumberByteSlices

instance AlgorandMessageUnpack StateSchema where
  fromCanonicalObject o = do
    ssNumberInts <- o .:? "nui"
    ssNumberByteSlices <- o .:? "nbs"
    pure StateSchema{..}


instance AlgorandMessagePack SignedTransaction where
  toCanonicalObject SignedTransaction{..} = mempty
    & "sig" .= stSig
    & "txn" .=< stTransaction

instance AlgorandMessageUnpack SignedTransaction where
  fromCanonicalObject o = do
    stSig <- o .: "sig"
    stTransaction <- o .:> "txn"
    pure SignedTransaction{..}
