-- SPDX-FileCopyrightText: 2021 Serokell <https://serokell.io>
--
-- SPDX-License-Identifier: MPL-2.0

-- | REST client for connecting to @algod@.
module Network.Algorand.Client
  ( AlgoNode (..)
  , connectToNode

  , AlgoIndexer (..)
  , connectToIndexer
  ) where

import qualified Data.Text as T

import Control.Exception.Safe (MonadThrow, throwM)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Network.HTTP.Client.TLS (newTlsManager)
import Servant.Client (mkClientEnv, parseBaseUrl, runClientM)
import Servant.Client.Generic (AsClientT, genericClientHoist)

import Network.Algorand.Api.Indexer (IndexerApi)
import Network.Algorand.Api.Node (NodeApi)
import Network.Algorand.Definitions (Host, Network)

newtype AlgoNode = AlgoNode
  { getAlgoNode  :: forall m . MonadIO m => NodeApi (AsClientT m)
  }

-- | Connect to a node and make sure it is working on the expected network.
connectToNode
  :: forall m. (MonadIO m, MonadThrow m)
  => Host
  -- ^ Host of the node.
  -> Network
  -- ^ Expected network (genesis id).
  -> m (Network, AlgoNode)
connectToNode host net = do
  manager <- newTlsManager
  env <- mkClientEnv manager <$> parseBaseUrl (T.unpack host)
  let
    apiNodeClient :: forall m' . MonadIO m' => NodeApi (AsClientT m')
    apiNodeClient = genericClientHoist $ \x ->
      liftIO $ runClientM x env >>= either throwM pure
  pure (net, AlgoNode apiNodeClient)

newtype AlgoIndexer = AlgoIndexer
  { getAlgoIndexer :: forall m . MonadIO m => IndexerApi (AsClientT m)
  }

-- | Connect to an indexer.
connectToIndexer
  :: forall m. (MonadIO m, MonadThrow m)
  => Host
  -- ^ Host of the indexer.
  -> Network
  -- ^ Expected network (genesis id).
  -> m (Network, AlgoIndexer)
connectToIndexer host net = do
  manager <- newTlsManager
  env <- mkClientEnv manager <$> parseBaseUrl (T.unpack host)
  let
    apiIndexerClient :: forall m' . MonadIO m' => IndexerApi (AsClientT m')
    apiIndexerClient = genericClientHoist $ \x ->
      liftIO $ runClientM x env >>= either throwM pure
  pure (net, AlgoIndexer apiIndexerClient)
