-- SPDX-FileCopyrightText: 2021 Serokell <https://serokell.io>
--
-- SPDX-License-Identifier: MPL-2.0

-- | @SignedTransaction@ and tools for creating/verifying it.
module Data.Algorand.Transaction.Signed
  ( SignedTransaction (..)

  , signSimple
  , signFromContractAccount

  , verifyTransaction
  , getSignature
  , getUnverifiedTransaction
  ) where

import qualified Data.Aeson as JS
import qualified Data.HashMap.Strict as HM

import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.ByteString (ByteString)
import Data.String (IsString)
import GHC.Generics (Generic)

import Crypto.Algorand.Key (SecretKey, toPublic)
import Crypto.Algorand.Signature (LogicSignature (..), Signature, SignatureType (..))
import Crypto.Algorand.Util (sign, verify)
import Data.Algorand.Address (fromContractCode, fromPublicKey, toPublicKey)
import Data.Algorand.MessagePack (MessagePackObject (toCanonicalObject),
                                  MessageUnpackObject (fromCanonicalObject), (&), (&<>), (.:>),
                                  (.=<))
import Data.Algorand.Transaction (Transaction (..), serialiseTx)

-- | A signed transaction object.
data SignedTransaction = SignedTransaction
  { stSig :: SignatureType
  , stTxn :: Transaction
  } deriving (Eq, Generic, Show)

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
getSignature :: SignedTransaction -> SignatureType
getSignature = stSig

-- | Dangerous: returns a transaction without verifying the signature.
getUnverifiedTransaction :: SignedTransaction -> Transaction
getUnverifiedTransaction = stTxn

signedTransactionFieldName :: IsString s => String -> s
signedTransactionFieldName = \case
  "stTxn" -> "txn"
  x -> error $ "Unmapped signed transaction field name: " <> x

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
      -- Delete the transaction field because the parser for
      -- TransactionSignature wants _exactly_ one field.
      stSig <- parseJSON $ JS.Object (HM.delete (f "stTxn") obj)
      stTxn <- (obj JS..: f "stTxn") >>= parseJSON
      pure SignedTransaction{..}
    where
      f = signedTransactionFieldName
