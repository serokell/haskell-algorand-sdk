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
import Servant.API.Generic (AsApi, GenericServant, ToServant)
import Servant.Client (Client, ClientEnv, ClientM, HasClient, mkClientEnv, parseBaseUrl, runClientM)
import Servant.Client.Generic (AsClientT, genericClientHoist)

import qualified Network.Algorand.Api as Api

import Network.Algorand.Api (IndexerApi(..), NodeApi(..), Health (..), Version (..))
import Network.Algorand.Definitions (Host, Network, NetworkError (WrongNetwork))

apiClient
  :: ( HasClient ClientM (ToServant routes AsApi)
     , GenericServant routes (AsClientT n)
     , Client n (ToServant routes AsApi) ~ ToServant routes (AsClientT n)
     , MonadIO n
     )
 => ClientEnv -> routes (AsClientT n)
apiClient env = genericClientHoist $ \x ->
  liftIO $ runClientM x env >>= either throwM pure

newtype AlgoNode = AlgoNode
  { getAlgoNode :: forall m . MonadIO m => NodeApi (AsClientT m)
  }

-- TODO: remove Network argument and return value OR restore network/version check
-- (we are waiting for a response from RandLabs).

-- | Connect to a node.
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
    apiV2Client :: forall m' . MonadIO m' => NodeApi (AsClientT m')
    apiV2Client = apiClient env

  Version{vGenesisId} <- _version apiV2Client
  case vGenesisId == net of
    True -> pure (net, AlgoNode apiV2Client)
    False -> throwM $ WrongNetwork net vGenesisId

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
  -> m (Network , AlgoIndexer)
connectToIndexer host net = do
  manager <- newTlsManager
  env <- mkClientEnv manager <$> parseBaseUrl (T.unpack host)
  let
    apiIdx2Client :: forall m' . MonadIO m' => IndexerApi (AsClientT m')
    apiIdx2Client = apiClient env

  Health{hGenesisId} <- _health apiIdx2Client
  case hGenesisId == net of
    True -> pure (net, AlgoIndexer apiIdx2Client)
    False ->  throwM $ WrongNetwork net hGenesisId
