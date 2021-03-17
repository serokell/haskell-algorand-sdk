-- SPDX-FileCopyrightText: 2021 Serokell <https://serokell.io>
--
-- SPDX-License-Identifier: MPL-2.0

-- | The REST API v2 of Algod, the Algorand node.
--
-- See <https://developer.algorand.org/docs/reference/rest-apis/algod/v2/>
module Network.Algorand.Node.Api
  ( Api (..)
  , ApiAny (..)
  , ApiV2 (..)
  , msgPackFormat
  , module M
  ) where

import GHC.Generics (Generic)

import qualified Data.Binary as Binary
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as BSL
import Data.Text (Text)
import Network.HTTP.Media ((//))
import Servant.API ((:>), Capture, Get, JSON, Post, ReqBody, QueryParam)
import qualified Servant.API.ContentTypes as Mime
import Servant.API.Generic (ToServantApi, (:-))

import qualified Data.Algorand.MessagePack as MP
import Data.Algorand.Address (Address)
import Data.Algorand.Transaction.Signed (SignedTransaction)
import Data.Algorand.Block
import Network.Algorand.Node.Api.Type as M

-- | The part of the API that does not depend on the version.
data ApiAny route = ApiAny
  { _health :: route
      :- "health"
      :> Get '[JSON] ()
  , _version :: route
      :- "versions"
      :> Get '[JSON] Version
  }
  deriving (Generic)

-- | Algod API (v2 only).
data ApiV2 route = ApiV2
  { _account :: route
      :- "accounts"
      :> Capture "address" Address
      :> QueryParam "round" Round
      :> Get '[JSON] Account
  , _transactions :: route
      :- "transactions"
      :> ReqBody '[Binary] [SignedTransaction]
      :> Post '[JSON] TransactionsRep
  , _transactionsRaw :: route
      :- "transactions"
      :> ReqBody '[Binary] ByteString
      :> Post '[JSON] TransactionsRep
  , _transactionsPending :: route
      :- "transactions"
      :> "pending"
      :> Capture "txId" Text
      :> Get '[JSON] TransactionInfo
  , _transactionsParams :: route
      :- "transactions"
      :> "params"
      :> Get '[JSON] SuggestedParams
  , _status :: route
      :- "status"
      :> Get '[JSON] NodeStatus
  , _block :: route
      :- "blocks"
      :> Capture "round" Round
      :> QueryParam "format" Text
      :> Get '[MsgPack] BlockWrapped
  , _compileTeal :: route
      :- "teal"
      :> "compile"
      :> ReqBody '[Mime.PlainText] Text
      :> Post '[JSON] TealCompilationResult
  }
  deriving (Generic)

msgPackFormat :: Maybe Text
msgPackFormat = Just "msgpack"

-- | Algod API.
data Api route = Api
  { _vAny :: route
      :- ToServantApi ApiAny
  , _v2 :: route
      :- "v2"
      :> ToServantApi ApiV2
  }
  deriving (Generic)

{-
 - Utils
 -}

-- | Content type for the `v2/transactions` endpoint.
data Binary
instance Mime.Accept Binary where
  contentType _ = "application" // "x-binary"
instance Mime.MimeRender Binary ByteString where
  mimeRender _ = BSL.fromStrict
instance Mime.MimeRender Binary BSL.ByteString where
  mimeRender _ = id
instance Mime.MimeRender Binary [SignedTransaction] where
  mimeRender _ = mconcat . map (MP.pack . MP.Canonical)

-- | Content type for the `v2/blocks` endpoint.
data MsgPack
instance Mime.Accept MsgPack where
  contentType _ = "application" // "msgpack"
instance Mime.MimeUnrender MsgPack BlockWrapped where
  mimeUnrender _ = parseMsgPack
    where
      eitherToM (Left  (_, _, msg)) = fail msg
      eitherToM (Right (_, _, res)) = pure res
      parseMsgPack bs = MP.runEitherError $
        eitherToM (Binary.decodeOrFail bs)
          >>= MP.fromAlgoObject . MP.unCompatObject
          >>= MP.fromCanonicalObject
