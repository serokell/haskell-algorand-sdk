-- SPDX-FileCopyrightText: 2021 Serokell <https://serokell.io>
--
-- SPDX-License-Identifier: MPL-2.0

-- | Convenience wrappers around the node API.
module Network.Algorand.Util
  ( TransactionStatus (..)
  , transactionStatus
  , getBlock
  , getBlockAtRound
  , getAccount
  , getAccountAtRound
  , lookupAssetBalance
  , lookupAppLocalState
  ) where

import qualified Data.Aeson as J
import qualified Data.HashMap.Strict as HM
import qualified Data.Map as M
import qualified Data.Text as T

import Control.Exception.Safe (MonadCatch, MonadThrow, handle, throwM)
import Data.ByteString (ByteString)
import Data.Map (Map)
import Data.Text (Text)
import Data.Word (Word64)
import Network.HTTP.Types (Status (statusCode))
import Servant.Client (ClientError (..), ResponseF (..))
import Servant.Client.Generic (AsClientT)

import qualified Data.Algorand.Block as B
import qualified Network.Algorand.Api as Api

import Data.Algorand.Address (Address)
import Data.Algorand.Round (Round)
import Data.Algorand.Teal (TealKeyValue (..), TealValue (..), tealValueBytesType, tealValueUintType)
import Data.Algorand.Transaction (AppIndex, AssetIndex)
import Network.Algorand.Api.Node (TransactionInfo (..))

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

-- | Catches error that has entity absence and return Nothing in this case.
-- Other errors would be rethrown.
noEntityHandler :: MonadThrow m => Text -> ClientError -> m (Maybe a)
noEntityHandler
  noEntityMsg
  (FailureResponse _req Response { responseStatusCode = s, responseBody = b })
    | statusCode s == 404 || statusCode s == 500
    , Just (J.Object errObj) <- J.decode' b
    , Just (J.String msg) <- HM.lookup "message" errObj
    , T.take (T.length noEntityMsg) msg == noEntityMsg
  = pure Nothing
noEntityHandler _ e = throwM e

{-# DEPRECATED getBlock "Use `getBlockAtRound` instead" #-}
-- | Helper to get block from node
getBlock
  :: MonadCatch m
  => Api.NodeApi (AsClientT m) -> Round -> m (Maybe B.Block)
getBlock api rnd = handle (noEntityHandler noBlockMsg) $ do
  B.BlockWrapped block <- Api._block api rnd Api.msgPackFormat
  pure (Just block)
  where
    noBlockMsg = "ledger does not have entry"

-- | Helper to get block from indexer
getBlockAtRound
  :: MonadCatch m
  => Api.IndexerApi (AsClientT m) -> Round -> m (Maybe Api.BlockResp)
getBlockAtRound api rnd = handle (noEntityHandler noBlockMsg) $ do
  Just <$> Api._blockIdx api rnd
  where
    noBlockMsg = "no blocks found"

getAccount
  :: MonadCatch m
  => Api.NodeApi (AsClientT m) -> Address -> m (Maybe Api.Account)
getAccount api addr = handle (noEntityHandler noAccMsg) $
  Just <$> Api._account api addr

getAccountAtRound
  :: MonadCatch m
  => Api.IndexerApi (AsClientT m)
  -> Address
  -> Maybe Round
  -> m (Maybe Api.IdxAccountResponse)
getAccountAtRound api addr rnd = handle (noEntityHandler noAccMsg) $
  Just <$> Api._accountIdx api addr rnd

noAccMsg :: Text
noAccMsg = "no accounts found for address"

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
    toEntry TealKeyValue{..}
      | tvType tkeValue == tealValueBytesType
      = (tkeKey, Left $ tvBytes tkeValue)
      | tvType tkeValue == tealValueUintType
      = (tkeKey, Right $ tvUint tkeValue)
      | otherwise = error $
        "lookupAppLocalState: unknown teal value type "
        <> show (tvType tkeValue)
