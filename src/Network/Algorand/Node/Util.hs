-- SPDX-FileCopyrightText: 2021 Serokell <https://serokell.io>
--
-- SPDX-License-Identifier: MPL-2.0

-- | Convenience wrappers around the node API.
module Network.Algorand.Node.Util
  ( TransactionStatus (..)
  , transactionStatus
  , getBlock
  ) where

import qualified Data.Aeson as J
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T

import Control.Exception.Safe (MonadCatch, handle, throwM)
import Data.Text (Text)
import Data.Word (Word64)
import Network.HTTP.Types (Status (statusCode))
import Servant.Client (ClientError (..), ResponseF (..))
import Servant.Client.Generic (AsClientT)

import qualified Data.Algorand.Block as B
import qualified Network.Algorand.Node.Api as Api

import Network.Algorand.Node.Api (TransactionInfo (..))


-- | Status of a transaction in the pool.
data TransactionStatus
  = Waiting  -- ^ Still in the pool waiting to be confirmed.
  | Confirmed Word64  -- ^ Transaction was confirmed at this round.
  | KickedOut Text  -- ^ It was kicked out of this node’s pool for this reason.

-- | Summarise 'TransactionInfo' as 'TransactionStatus'.
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
    noBlockMsg = "ledger does not have entry"
    handler (FailureResponse _req
              Response { responseStatusCode = s, responseBody = b })
      | statusCode s == 500
      , Just (J.Object errObj) <- J.decode' b
      , Just (J.String msg) <- HM.lookup "message" errObj
      , T.take (T.length noBlockMsg) msg == noBlockMsg
      = pure Nothing
    handler e = throwM e
