-- SPDX-FileCopyrightText: 2021 Serokell <https://serokell.io>
--
-- SPDX-License-Identifier: MPL-2.0

-- | @SignedTransaction@ and tools for creating/verifying it.
module Data.Algorand.Transaction.Signed
  ( TransactionSignature (..)
  , SignedTransaction ()

  , signSimple
  , signFromContractAccount

  , verifyTransaction
  , getSignature
  , getUnverifiedTransaction

  , BlockTransaction
  , toSignedTransaction
  ) where

import qualified Data.Aeson as JS
import qualified Data.HashMap.Strict as HM

import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.ByteString (ByteString)
import Data.String (IsString)
import Data.Text (Text)
import GHC.Generics (Generic)

import Crypto.Algorand.Signature (SecretKey, Signature, sign, toPublic, verify)
import Data.Algorand.Address (fromContractCode, fromPublicKey, toPublicKey)
import Data.Algorand.MessagePack (MessagePackObject (toCanonicalObject),
                                  MessageUnpackObject (fromCanonicalObject),
                                  NonZeroValue (isNonZero), (&), (&<>), (.:>), (.:>?), (.:?),
                                  (.:??), (.=), (.=<))
import Data.Algorand.MessagePack.Json (parseCanonicalJson, toCanonicalJson)
import Data.Algorand.Transaction (GenesisHash, Transaction (..), serialiseTx)

-- | Types of transaction signatures.
data TransactionSignature
  = SignatureSimple Signature
  | SignatureMulti MultiSignature
  | SignatureLogic LogicSignature
  deriving (Eq, Generic, Show)

instance NonZeroValue TransactionSignature where
  isNonZero _ = True

-- | A signed transaction object.
data SignedTransaction = SignedTransaction
  { stSig :: TransactionSignature
  , stTxn :: Transaction
  } deriving (Eq, Generic, Show)


{- Simple signature -}

-- | Sign a transaction with a simple signature.
--
-- Note: this function will overwrite the sender of the transaction!
signSimple :: SecretKey -> Transaction -> SignedTransaction
signSimple sk txn =
  let txn' = txn { tSender = fromPublicKey (toPublic sk) }
  in SignedTransaction
    { stTxn = txn'
    , stSig = SignatureSimple $ sign sk (serialiseTx txn')
    }

-- | Verify a simple signature transaction.
verifySimple :: Signature -> Transaction -> Bool
verifySimple sig txn =
  case toPublicKey (tSender txn) of
    Nothing -> False
    Just pk -> verify pk (serialiseTx txn) sig


{- Multi signature -}

data MultiSignature = MultiSignature
  deriving (Eq, Generic, Show)

instance NonZeroValue MultiSignature where
  isNonZero _ = True


{- Logic signature -}

data LogicSignature = ContractAccountSignature
  -- TODO: Only contract account signature is supported.
  { lsLogic :: ByteString
  , lsArgs :: [ByteString]
  }
  deriving (Eq, Generic, Show)

instance NonZeroValue LogicSignature where
  isNonZero _ = True

-- | Sign a transaction from a contract account.
--
-- Note: this function will overwrite the sender of the transaction!
signFromContractAccount
  :: ByteString  -- ^ Compiled contract code.
  -> [ByteString]  -- ^ Program arguments.
  -> Transaction  -- ^ Transaction to sign.
  -> SignedTransaction
signFromContractAccount lsLogic lsArgs txn = SignedTransaction{..}
  where
    stTxn = txn { tSender = fromContractCode lsLogic }
    stSig = SignatureLogic $ ContractAccountSignature{lsLogic, lsArgs}

-- | Verify a signed transaction.
verifyTransaction :: SignedTransaction -> Maybe Transaction
verifyTransaction SignedTransaction{..} =
  let
    ok = case stSig of
      SignatureSimple sig -> verifySimple sig stTxn
      SignatureMulti _msig -> False  -- FIXME
      SignatureLogic lsig -> case lsig of
        ContractAccountSignature{lsLogic} ->
          tSender stTxn == fromContractCode lsLogic
  in case ok of
    False -> Nothing
    True -> Just stTxn

-- | Get the signature.
getSignature :: SignedTransaction -> TransactionSignature
getSignature = stSig

-- | Dangerous: returns a transaction without verifying the signature.
getUnverifiedTransaction :: SignedTransaction -> Transaction
getUnverifiedTransaction = stTxn



{-
 - Horrible hand-written serialisation code.
 -}

signedTransactionFieldName :: IsString s => String -> s
signedTransactionFieldName = \case
  "stTxn" -> "txn"
  x -> error $ "Unmapped signed transaction field name: " <> x

transactionSignatureType :: IsString s => String -> s
transactionSignatureType = \case
  "SignatureSimple" -> "sig"
  "SignatureMulti" -> "msig"
  "SignatureLogic" -> "lsig"
  x -> error $ "Unmapped transaction signature constructor: " <> x

instance MessagePackObject SignedTransaction where
  toCanonicalObject SignedTransaction{..} = mempty
      & f "stTxn" .=< stTxn
      &<> stSig
    where
      f = signedTransactionFieldName

instance MessageUnpackObject SignedTransaction where
  fromCanonicalObject o = do
      stTxn <- o .:> f "stTxn"
      stSig <- fromCanonicalObject o
      pure SignedTransaction{..}
    where
      f = signedTransactionFieldName

instance ToJSON SignedTransaction where
  toJSON SignedTransaction{..} = JS.Object $
      sigObject <>
      HM.fromList  -- note: inlining into the same object
        [ (f "stTxn", toJSON stTxn)
        ]
    where
      f = signedTransactionFieldName

      sigObject = case toJSON stSig of
        JS.Object hm -> hm
        _ -> error "Incorrect encoding for TransactionSignature"

instance FromJSON SignedTransaction where
  parseJSON = JS.withObject "SignedTransaction" $ \obj -> do
      -- Delete the tranaction field because the parser for
      -- TransactionSignature wants _exactly_ one field.
      stSig <- parseJSON $ JS.Object (HM.delete (f "stTxn") obj)
      stTxn <- (obj JS..: f "stTxn") >>= parseJSON
      pure SignedTransaction{..}
    where
      f = signedTransactionFieldName

instance MessagePackObject TransactionSignature where
  toCanonicalObject = \case
      SignatureSimple sig -> mempty
        & t "SignatureSimple" .= sig
      SignatureMulti msig -> mempty
        & t "SignatureMulti" .=< msig
      SignatureLogic lsig -> mempty
        & t "SignatureLogic" .=< lsig
    where
      t = transactionSignatureType :: String -> Text

instance MessageUnpackObject TransactionSignature where
  fromCanonicalObject o = o .:?? t "SignatureSimple" >>= \case
      Just sig -> pure $ SignatureSimple sig
      Nothing -> o .:>? t "SignatureMulti" >>= \case
        Just msig -> pure $ SignatureMulti msig
        Nothing -> o .:>? t "SignatureLogic" >>= \case
          Just lsig -> pure $ SignatureLogic lsig
          Nothing -> fail "Unsupported or missing signature"
    where
      t = transactionSignatureType :: String -> Text

instance ToJSON TransactionSignature where
  toJSON = toCanonicalJson

instance FromJSON TransactionSignature where
  parseJSON = parseCanonicalJson

--multiSignatureFieldName :: IsString s => String -> s
--multiSignatureFieldName = \case
--  x -> error $ "Unmapped multi signature field name: " <> x

instance MessagePackObject MultiSignature where
  toCanonicalObject MultiSignature = mempty  -- TODO

instance MessageUnpackObject MultiSignature where
  fromCanonicalObject _ = pure MultiSignature  -- TODO

instance ToJSON MultiSignature where
  toJSON = toCanonicalJson

instance FromJSON MultiSignature where
  parseJSON = parseCanonicalJson

logicSignatureFieldName :: IsString s => String -> s
logicSignatureFieldName = \case
  "lsLogic" -> "l"
  "lsArgs" -> "arg"
  x -> error $ "Unmapped logic signature field name: " <> x

instance MessagePackObject LogicSignature where
  toCanonicalObject = \case
      ContractAccountSignature{..} -> mempty
        & f "lsLogic" .= lsLogic
        & f "lsArgs" .= lsArgs
    where
      f = logicSignatureFieldName

instance MessageUnpackObject LogicSignature where
  fromCanonicalObject o = do
      lsLogic <- o .:? f "lsLogic"
      lsArgs <- o .:? f "lsArgs"
      pure ContractAccountSignature{..}
    where
      f = logicSignatureFieldName

instance ToJSON LogicSignature where
  toJSON = toCanonicalJson

instance FromJSON LogicSignature where
  parseJSON = parseCanonicalJson

data BlockTransaction = BlockTransaction
  { btSig :: TransactionSignature
  , btTxn :: Transaction
  , btHgh :: Bool
  -- ^ [btHgh] whether the tx has genesis hash included in serialized representation.
  , btHgi :: Bool
  -- ^ [btHgi] whether the tx has genesis id included in serialized representation.
  -- , btApplyData :: ApplyData
  } deriving (Eq, Generic, Show)

instance MessageUnpackObject BlockTransaction where
  fromCanonicalObject o = do
    btTxn <- o .:> "txn"
    btSig <- fromCanonicalObject o
    btHgi <- o .:? "hgi"
    btHgh <- o .:? "hgh"
    pure BlockTransaction{..}

instance MessagePackObject BlockTransaction where
  toCanonicalObject BlockTransaction{..} = mempty
      & "txn" .=< btTxn
      & "hgi" .= btHgi
      & "hgh" .= btHgh
      &<> btSig

-- | Convert block tx to signed tx
toSignedTransaction
  :: Bool
  -- ^ Is genesis hash required (parameter of consensus protocol)
  -> GenesisHash
  -> Text
  -- ^ Genesis id
  -> BlockTransaction
  -> SignedTransaction
toSignedTransaction requireGH gh gid BlockTransaction{..} =
  SignedTransaction
    { stSig = btSig
    , stTxn = btTxn
        { tGenesisId =
            if btHgi
            then Just gid
            else tGenesisId btTxn
        , tGenesisHash =
            if btHgh || requireGH
            then Just gh
            else tGenesisHash btTxn
        }
    }
