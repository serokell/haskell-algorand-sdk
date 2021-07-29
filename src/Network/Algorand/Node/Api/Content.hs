-- SPDX-FileCopyrightText: 2021 Serokell <https://serokell.io>
--
-- SPDX-License-Identifier: MPL-2.0

-- | Custom content types for API
module Network.Algorand.Node.Api.Content
  ( Binary
  , MsgPack
  , msgPackFormat
  ) where

import qualified Data.Binary as Binary
import qualified Data.ByteString.Lazy as BSL
import qualified Servant.API.ContentTypes as Mime

import Data.ByteString (ByteString)
import Data.Text (Text)
import Network.HTTP.Media ((//))

import qualified Data.Algorand.MessagePack as MP

import Data.Algorand.Transaction.Signed (SignedTransaction)

-- | Content type for the `/transactions` endpoint.
data Binary

instance Mime.Accept Binary where
  contentType _ = "application" // "x-binary"

instance Mime.MimeRender Binary ByteString where
  mimeRender _ = BSL.fromStrict

instance Mime.MimeRender Binary BSL.ByteString where
  mimeRender _ = id

instance Mime.MimeRender Binary [SignedTransaction] where
  mimeRender _ = mconcat . map (MP.pack . MP.Canonical)

-- | Content type for the endpoints which can return msgpack.
data MsgPack

-- | Helper to set MsgPack format, can be passed as format parameter
msgPackFormat :: Maybe Text
msgPackFormat = Just "msgpack"

instance Mime.Accept MsgPack where
  contentType _ = "application" // "msgpack"

instance MP.MessageUnpackObject a => Mime.MimeUnrender MsgPack a where
  mimeUnrender _ bs = MP.runEitherError $
    eitherToM (Binary.decodeOrFail bs)
      >>= MP.fromAlgoObject . MP.unCompatObject
      >>= MP.fromCanonicalObject
    where
      eitherToM (Left  (_, _, msg)) = fail msg
      eitherToM (Right (_, _, res)) = pure res
