-- SPDX-FileCopyrightText: 2021 Serokell <https://serokell.io>
--
-- SPDX-License-Identifier: MPL-2.0

-- | Convenience wrappers around the node API.
module Network.Algorand.Util
  ( TransactionStatus (..)
  , transactionStatus
  , getBlock
  , lookupAssetBalance
  , lookupAppLocalState
  ) where

import qualified Data.Aeson as J
import qualified Data.HashMap.Strict as HM
import qualified Data.Map as M
import qualified Data.Text as T

import Control.Exception.Safe (MonadCatch, handle, throwM)
import Data.ByteString (ByteString)
import Data.Map (Map)
import Data.Text (Text)
import Data.Word (Word64)
import Network.HTTP.Types (Status (statusCode))
import Servant.Client (ClientError (..), ResponseF (..))
import Servant.Client.Generic (AsClientT)

import qualified Data.Algorand.Block as B
import qualified Network.Algorand.Api as Api

import Data.Algorand.Transaction (AppIndex, AssetIndex)
import Network.Algorand.Api.Type (TransactionInfo (..))

-- | Status of a transaction in the pool.
data TransactionStatus
  = Waiting  -- ^ Still in the pool waiting to be confirmed.
  | Confirmed Word64  -- ^ Transaction was confirmed at this round.
  | KickedOut Text  -- ^ It was kicked out of this node’s pool for this reason.

-- | Summarize 'TransactionInfo' as 'TransactionStatus'.
transactionStatus :: TransactionInfo -> TransactionStatus
transactionStatus TransactionInfo{tiConfirmedRound, tiPoolError} =
  case tiConfirmedRound of
    Just r -> Confirmed r
    Nothing -> case T.null tiPoolError of
      False -> KickedOut tiPoolError
      True -> Waiting

-- | Catches error that has block absence for the round as the cause and
-- return Nothing in this case. Other errors would be rethrown.
getBlock
  :: MonadCatch m
  => Api.ApiV2 (AsClientT m) -> B.Round -> m (Maybe B.Block)
getBlock api rnd = handle handler $ do
  B.BlockWrapped block <- Api._block api rnd Api.msgPackFormat
  pure (Just block)
  where
    -- note: this message depends on format, this one is for msgpack
    noBlockMsg = "ledger does not have entry"
    handler (FailureResponse _req
              Response { responseStatusCode = s, responseBody = b })
      | statusCode s == 500
      , Just (J.Object errObj) <- J.decode' b
      , Just (J.String msg) <- HM.lookup "message" errObj
      , T.take (T.length noBlockMsg) msg == noBlockMsg
      = pure Nothing
    handler e = throwM e

-- | Helper to get asset balance at account
lookupAssetBalance :: Api.Account -> AssetIndex -> Word64
lookupAssetBalance Api.Account{..} assetId
  | Just Api.Asset{..} <- aAssets >>= lookup assetId . map toPair
  , not asIsFrozen = asAmount
  | otherwise = 0
  where
    toPair a@Api.Asset{..} = (asAssetId, a)

-- | Helper to get account local state
lookupAppLocalState
  :: Api.Account
  -> AppIndex
  -> Maybe (Map ByteString (Either ByteString Word64))
lookupAppLocalState Api.Account{..} appId = do
  Api.LocalState{..} <- aAppsLocalState >>= lookup appId . map toPair
  M.fromList . map toEntry <$> lsKeyValue
  where
    toPair a@Api.LocalState{..} = (lsId, a)
    toEntry Api.TealKeyValue{..}
      | Api.tvType tkeValue == Api.tealValueBytesType
      = (tkeKey, Left $ Api.tvBytes tkeValue)
      | Api.tvType tkeValue == Api.tealValueUintType
      = (tkeKey, Right $ Api.tvUint tkeValue)
      | otherwise = error $
        "lookupAppLocalState: unknown teal value type "
        <> show (Api.tvType tkeValue)
