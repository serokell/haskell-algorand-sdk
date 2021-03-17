-- SPDX-FileCopyrightText: 2021 Serokell <https://serokell.io>
--
-- SPDX-License-Identifier: MPL-2.0

-- | @SignedTransaction@ and tools for creating/verifying it.
module Data.Algorand.Transaction.Signed
  ( TransactionSignature (..)
  , SignedTransaction ()

  , signSimple

  , verifyTransaction
  , getSignature
  , getUnverifiedTransaction
  ) where

import Data.Aeson (FromJSON (..), ToJSON (..))
import qualified Data.Aeson as JS
import qualified Data.HashMap.Strict as HM
import Data.String (IsString)
import Data.Text (Text)
import GHC.Generics (Generic)

import Crypto.Algorand.Signature (SecretKey, Signature, sign, verify)
import Data.Algorand.Address (Address, toPublicKey)
import Data.Algorand.MessagePack (MessagePackObject (toCanonicalObject), MessageUnpackObject (fromCanonicalObject), Canonical (Canonical), (&), (&<>), (.=), (.=<), (.:), (.:?), (.:??), (.:>), (.:>?), NonZeroValue (isNonZero))
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
  -- TODO: Only simple signature is supported for now.
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
  let pk = toPublicKey (tSender txn) in
  verify pk (serialiseTx txn) sig


{- Multi signature -}

data MultiSignature = MultiSignature
  deriving (Generic, Show)

instance NonZeroValue MultiSignature where
  isNonZero _ = True


{- Logic signature -}

data LogicSignature = LogicSignature
  deriving (Generic, Show)

instance NonZeroValue LogicSignature where
  isNonZero _ = True


-- | Verify a signed transaction.
verifyTransaction :: SignedTransaction -> Maybe Transaction
verifyTransaction SignedTransaction{..} =
  let
    ok = case stSig of
      SignatureSimple sig -> verifySimple sig stTxn
      SignatureMulti _msig -> False  -- FIXME
      SignatureLogic _lsig -> False -- FIXME
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


multiSignatureFieldName :: IsString s => String -> s
multiSignatureFieldName = \case
  x -> error $ "Unmapped multi signature field name: " <> x

instance MessagePackObject MultiSignature where
  toCanonicalObject MultiSignature = mempty  -- TODO

instance MessageUnpackObject MultiSignature where
  fromCanonicalObject _ = pure MultiSignature  -- TODO

multiSignatureJsonOptions :: JS.Options
multiSignatureJsonOptions = defaultOptions { JS.fieldLabelModifier = multiSignatureFieldName }

instance ToJSON MultiSignature where
  toJSON = JS.genericToJSON multiSignatureJsonOptions
  toEncoding = JS.genericToEncoding multiSignatureJsonOptions

instance FromJSON MultiSignature where
  parseJSON = JS.genericParseJSON multiSignatureJsonOptions


logicSignatureFieldName :: IsString s => String -> s
logicSignatureFieldName = \case
  x -> error $ "Unmapped logic signature field name: " <> x

instance MessagePackObject LogicSignature where
  toCanonicalObject LogicSignature = mempty  -- TODO

instance MessageUnpackObject LogicSignature where
  fromCanonicalObject _ = pure LogicSignature  -- TODO

logicSignatureJsonOptions :: JS.Options
logicSignatureJsonOptions = defaultOptions { JS.fieldLabelModifier = logicSignatureFieldName }

instance ToJSON LogicSignature where
  toJSON = JS.genericToJSON logicSignatureJsonOptions
  toEncoding = JS.genericToEncoding logicSignatureJsonOptions

instance FromJSON LogicSignature where
  parseJSON = JS.genericParseJSON logicSignatureJsonOptions
