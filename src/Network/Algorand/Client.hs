-- SPDX-FileCopyrightText: 2021 Serokell <https://serokell.io>
--
-- SPDX-License-Identifier: MPL-2.0

-- | REST client for connecting to @algod@.
module Network.Algorand.Client
  ( AlgoNode (..)
  , connectToNode

  , AlgoIndexer (..)
  , connectToIndexer

  , module Api
  ) where

import qualified Data.Text as T

import Control.Exception.Safe (MonadThrow, throwM)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Network.HTTP.Client.TLS (newTlsManager)
import Servant.API.Generic (fromServant)
import Servant.Client (ClientEnv, mkClientEnv, parseBaseUrl, runClientM)
import Servant.Client.Generic (AsClientT, genericClientHoist)

import qualified Network.Algorand.Api as Api

import Network.Algorand.Api (Api (..), ApiAny (..), ApiIdx2, ApiV2, Version (..))
import Network.Algorand.Definitions (Host, Network, NetworkError (WrongNetwork))

apiClient :: forall m' . MonadIO m' => ClientEnv -> Api (AsClientT m')
apiClient env = genericClientHoist $
  \x -> liftIO $ runClientM x env >>= either throwM pure

newtype AlgoNode = AlgoNode
  { getAlgoNode  :: forall m' . MonadIO m' => ApiV2 (AsClientT m')
  }

-- | Connect to a node and make sure it is working on the expected network.
connectToNode
  :: forall m. (MonadIO m, MonadThrow m)
  => Host
  -- ^ Host of the node.
  -> Network
  -- ^ Expected network (genesis id).
  -> m (Api.Version, AlgoNode)
connectToNode host net = do
  manager <- newTlsManager
  env <- mkClientEnv manager <$> parseBaseUrl (T.unpack host)
  let
    apiAny :: ApiAny (AsClientT m)
    apiAny = fromServant $ _vAny $ apiClient env

    apiV2Client :: forall m' . MonadIO m' => ApiV2 (AsClientT m')
    apiV2Client = fromServant $ _v2 $ apiClient env

  version@Version{vGenesisId} <- _version apiAny
  if vGenesisId == net
  then pure (version, AlgoNode apiV2Client)
  else throwM $ WrongNetwork net vGenesisId

newtype AlgoIndexer = AlgoIndexer
  { getAlgoIndexer :: forall m' . MonadIO m' => ApiIdx2 (AsClientT m')
  }

-- | Connect to an indexer.
connectToIndexer
  :: forall m. (MonadIO m, MonadThrow m)
  => Host
  -- ^ Host of the indexer.
  -> Network
  -- ^ Expected network (genesis id).
  -> m (Network , AlgoIndexer)
connectToIndexer host net = do
  manager <- newTlsManager
  env <- mkClientEnv manager <$> parseBaseUrl (T.unpack host)
  let
    apiIdx2Client :: forall m' . MonadIO m' => ApiIdx2 (AsClientT m')
    apiIdx2Client = fromServant $ _idx2 $ apiClient env

  pure (net, AlgoIndexer apiIdx2Client)
