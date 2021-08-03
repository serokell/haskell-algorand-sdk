-- SPDX-FileCopyrightText: 2021 Serokell <https://serokell.io>
--
-- SPDX-License-Identifier: MPL-2.0

-- | The REST API v2 of Algod, the Algorand node.
--
-- See <https://developer.algorand.org/docs/reference/rest-apis/algod/v2/>
module Network.Algorand.Api
  ( Api (..)
  , ApiAny (..)
  , ApiV2 (..)
  , ApiIdx2 (..)

  , module Content
  , module Type
  ) where

import Data.ByteString (ByteString)
import Data.Text (Text)
import GHC.Generics (Generic)
import Servant.API (Capture, Get, JSON, PlainText, Post, QueryParam, ReqBody, (:>))
import Servant.API.Generic (ToServantApi, (:-))

import Data.Algorand.Address (Address)
import Data.Algorand.Block (BlockWrapped, Round)
import Data.Algorand.Transaction.Signed (SignedTransaction)
import Network.Algorand.Api.Content as Content
import Network.Algorand.Api.Type as Type

-- | Algod API.
data Api route = Api
  { _vAny :: route
      :- ToServantApi ApiAny
  , _v2 :: route
      :- "v2"
      :> ToServantApi ApiV2
  , _idx2 :: route
      :- "v2"
      :> ToServantApi ApiIdx2
  } deriving (Generic)

-- | The part of the API that does not depend on the version.
data ApiAny route = ApiAny
  { _health :: route
      :- "health"
      :> Get '[JSON] ()
  , _version :: route
      :- "versions"
      :> Get '[JSON] Version
  } deriving (Generic)

-- | Algod API (v2 only).
data ApiV2 route = ApiV2
  { _status :: route
      :- "status"
      :> Get '[JSON] NodeStatus
  , _block :: route
      :- "blocks"
      :> Capture "round" Round
      :> QueryParam "format" Text
      -- do not try passing format other than msgpack here
      :> Get '[MsgPack] BlockWrapped
  , _account :: route
      :- "accounts"
      :> Capture "address" Address
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
  , _compileTeal :: route
      :- "teal"
      :> "compile"
      :> ReqBody '[PlainText] Text
      :> Post '[JSON] TealCompilationResult
  } deriving (Generic)

newtype ApiIdx2 route = ApiIdx2
  { _accountIdx :: route
      :- "accounts"
      :> Capture "address" Address
      :> QueryParam "round" Round
      :> Get '[JSON] IdxAccountResponse
  } deriving (Generic)
