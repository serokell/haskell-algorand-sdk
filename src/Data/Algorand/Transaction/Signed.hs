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
  ) where

import Data.Aeson (FromJSON (..), ToJSON (..))
import qualified Data.Aeson as JS
import Data.ByteString (ByteString)
import qualified Data.HashMap.Strict as HM
import Data.String (IsString)
import Data.Text (Text)
import GHC.Generics (Generic)

import Crypto.Algorand.Signature (SecretKey, Signature, sign, verify)
import Data.Algorand.Address (fromContractCode, toPublicKey)
import Data.Algorand.MessagePack (MessagePackObject (toCanonicalObject), MessageUnpackObject (fromCanonicalObject), (&), (&<>), (.=), (.=<), (.:?), (.:??), (.:>), (.:>?), NonZeroValue (isNonZero))
import Data.Algorand.Transaction (Transaction (..), serialiseTx)
import Network.Algorand.Node.Api.Json (defaultOptions)


-- | Types of transaction signatures.
data TransactionSignature
  = SignatureSimple Signature
  | SignatureMulti MultiSignature
  | SignatureLogic LogicSignature
  deriving (Generic, Show)

instance NonZeroValue TransactionSignature where
  isNonZero _ = True

-- | A signed transaction object.
data SignedTransaction = SignedTransaction
  { stSig :: TransactionSignature
  , stTxn :: Transaction
  }
  deriving (Generic, Show)


{- Simple signature -}

-- | Sign a transaction with a simple signature.
signSimple :: SecretKey -> Transaction -> SignedTransaction
signSimple sk txn = SignedTransaction
  { stTxn = txn
  , stSig = SignatureSimple $ sign sk (serialiseTx txn)
  }

-- | Verify a simple signature transaction.
verifySimple :: Signature -> Transaction -> Bool
verifySimple sig txn =
  case toPublicKey (tSender txn) of
    Nothing -> False
    Just pk -> verify pk (serialiseTx txn) sig


{- Multi signature -}

data MultiSignature = MultiSignature
  deriving (Generic, Show)

instance NonZeroValue MultiSignature where
  isNonZero _ = True


{- Logic signature -}

data LogicSignature = ContractAccountSignature
  -- TODO: Only contract account signature is supported.
  { lsLogic :: ByteString
  , lsArgs :: [ByteString]
  }
  deriving (Generic, Show)

instance NonZeroValue LogicSignature where
  isNonZero _ = True

-- | Sign a transaction from a contract account.
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
  parseJSON v = JS.withObject "SignedTransaction" (\obj -> do
      stSig <- parseJSON v
      stTxn <- (obj JS..: f "stTxn") >>= parseJSON
      pure SignedTransaction{..}
      ) v
    where
      f = signedTransactionFieldName


transactionSignatureJsonOptions :: JS.Options
transactionSignatureJsonOptions = defaultOptions
  { JS.constructorTagModifier = transactionSignatureType
  , JS.sumEncoding = JS.ObjectWithSingleField
  }

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
  toJSON = JS.genericToJSON transactionSignatureJsonOptions
  toEncoding = JS.genericToEncoding transactionSignatureJsonOptions

instance FromJSON TransactionSignature where
  parseJSON = JS.genericParseJSON transactionSignatureJsonOptions


--multiSignatureFieldName :: IsString s => String -> s
--multiSignatureFieldName = \case
--  x -> error $ "Unmapped multi signature field name: " <> x

instance MessagePackObject MultiSignature where
  toCanonicalObject MultiSignature = mempty  -- TODO

instance MessageUnpackObject MultiSignature where
  fromCanonicalObject _ = pure MultiSignature  -- TODO

multiSignatureJsonOptions :: JS.Options
multiSignatureJsonOptions = defaultOptions -- { JS.fieldLabelModifier = multiSignatureFieldName }

instance ToJSON MultiSignature where
  toJSON = JS.genericToJSON multiSignatureJsonOptions
  toEncoding = JS.genericToEncoding multiSignatureJsonOptions

instance FromJSON MultiSignature where
  parseJSON = JS.genericParseJSON multiSignatureJsonOptions


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

logicSignatureJsonOptions :: JS.Options
logicSignatureJsonOptions = defaultOptions { JS.fieldLabelModifier = logicSignatureFieldName }

instance ToJSON LogicSignature where
  toJSON = JS.genericToJSON logicSignatureJsonOptions
  toEncoding = JS.genericToEncoding logicSignatureJsonOptions

instance FromJSON LogicSignature where
  parseJSON = JS.genericParseJSON logicSignatureJsonOptions
