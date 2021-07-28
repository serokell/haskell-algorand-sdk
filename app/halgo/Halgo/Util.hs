-- SPDX-FileCopyrightText: 2021 Serokell <https://serokell.io>
--
-- SPDX-License-Identifier: MPL-2.0

-- | Utils.
module Halgo.Util
  ( die
  , handleApiError
  , withNode
  ) where

import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as T
import qualified System.Exit (die)

import Control.Exception.Safe (MonadCatch, handle)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (asks)
import Data.Bifunctor (second)
import Data.ByteString (ByteString)
import Data.Text.Encoding (decodeUtf8With)
import Data.Text.Encoding.Error (lenientDecode)
import Fmt (Builder, build, fmt, (+|), (+||), (|+), (||+))
import Network.HTTP.Types (Status (statusCode, statusMessage))
import Servant.Client (ClientError (..), ResponseF (..))
import Servant.Client.Generic (AsClientT)

import Network.Algorand.Node (NodeUrl, connect, getAlgoClient)
import Network.Algorand.Node.Api (ApiV2)

import qualified Network.Algorand.Node.Api as Api

import Halgo.CLA.Type (MonadSubCommand, goNetwork)

-- | @die@ that takes a builder and is lifted to MonadIO.
die :: MonadIO m => Builder -> m a
die = liftIO . System.Exit.die . fmt

-- | Try our best to decode bytes as UTF-8.
pBytes :: ByteString -> Builder
pBytes = build . T.stripEnd . decodeUtf8With lenientDecode

-- | Try our best to decode lazy bytes as UTF-8.
pBytesL :: BSL.ByteString -> Builder
pBytesL = pBytes . BSL.toStrict

-- | Handle and pretty-print servant-client errors.
handleApiError :: forall m a. (MonadIO m, MonadCatch m) => m a -> m a
handleApiError = handle showErr
  where
    showErr :: ClientError -> m a
    showErr (FailureResponse _req Response{responseStatusCode = s, responseBody = b}) = die $
      "Response "+||statusCode s||+" ("+|pBytes (statusMessage s)|+"): "+|pBytesL b|+""
    showErr (DecodeFailure err resp) = die $
      "Could not decode response: "+|err|+"\n"+||resp||+""
    showErr (UnsupportedContentType t _resp) = die $
      "Unsupported content type: "+||t||+""
    showErr x = die $ ""+||x||+""

-- | Connect to a node and check that its network is what we expect.
withNode
  :: MonadSubCommand m
  => NodeUrl
  -> ((Api.Version, ApiV2 (AsClientT m)) -> m a)
  -> m a
withNode url act = do
  net <- asks goNetwork
  connect url net >>= handleApiError . act . second getAlgoClient
