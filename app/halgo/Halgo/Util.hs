-- SPDX-FileCopyrightText: 2021 Serokell <https://serokell.io>
--
-- SPDX-License-Identifier: MPL-2.0

-- | Utils.
module Halgo.Util
  ( die

  , putTextLn
  , printJson

  , handleApiError
  ) where

import Control.Exception.Safe (MonadCatch, handle)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Aeson (ToJSON)
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as BSL
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Text.Encoding (decodeUtf8, decodeUtf8With)
import Data.Text.Encoding.Error (lenientDecode)
import Fmt (Builder, (|+), (+|), (||+), (+||), build, fmt)
import Network.HTTP.Types (Status (statusCode, statusMessage))
import Servant.Client (ClientError (..), ResponseF (..))
import qualified System.Exit (die)


-- | @die@ that takes a builder and is lifted to MonadIO.
die :: MonadIO m => Builder -> m a
die = liftIO . System.Exit.die . fmt

-- | @putStrLn@ for 'Text'.
putTextLn :: MonadIO m => Text -> m ()
putTextLn = liftIO . T.putStrLn

-- | Encode as JSON and pretty-print to stdout.
printJson :: (MonadIO m, ToJSON a) => a -> m ()
printJson = putTextLn . decodeUtf8 . BSL.toStrict . encodePretty

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
