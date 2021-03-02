-- SPDX-FileCopyrightText: 2021 Serokell <https://serokell.io>
--
-- SPDX-License-Identifier: MPL-2.0

-- | REST client for connecting to @algod@.
module Network.Algorand.Node
  ( connect
  , NodeUrl
  , BadNode (..)

  , module Api
  ) where

import Control.Exception.Safe (Exception, MonadThrow, throwM)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Text (Text)
import qualified Data.Text as T
import Network.HTTP.Client.TLS (newTlsManager)
import Servant.API.Generic (fromServant)
import Servant.Client (mkClientEnv, parseBaseUrl, runClientM)
import Servant.Client.Generic (AsClientT, genericClientHoist)

import Network.Algorand.Node.Api (Api (..), ApiV2, ApiAny (..), Version (..))
import qualified Network.Algorand.Node.Api as Api


-- | URL of the node to connect to.
type NodeUrl = Text


-- | Error thrown if 'connect' fails.
data BadNode = WrongNetwork
  { wnExpected :: Text
  , wnActual :: Text
  }

instance Show BadNode where
  show (WrongNetwork{wnExpected, wnActual}) = mconcat
    [ "Bad node: the node is connected to `" <> T.unpack wnActual <> "`, "
    , "we expected `" <> T.unpack wnExpected <> "`."
    ]

instance Exception BadNode


-- | Connect to a node and make sure it is working on the expected network.
connect
  :: forall m. (MonadIO m, MonadThrow m)
  => NodeUrl  -- ^ URL of the node.
  -> Text  -- ^ Expected network (genesis id).
  -> m (Api.Version, ApiV2 (AsClientT m))
connect url net = do
    manager <- newTlsManager
    env <- mkClientEnv manager <$> parseBaseUrl (T.unpack url)

    let
      apiClient :: Api (AsClientT m)
      apiClient = genericClientHoist (\x -> liftIO $ runClientM x env >>= either throwM pure)

      apiAny :: ApiAny (AsClientT m)
      apiAny = fromServant $ _vAny apiClient

      apiV2Client :: ApiV2 (AsClientT m)
      apiV2Client = fromServant $ _v2 apiClient

    version@Version{vGenesisId} <- _version apiAny
    case vGenesisId == net of
      False -> throwM $ WrongNetwork net vGenesisId
      True -> pure (version, apiV2Client)
