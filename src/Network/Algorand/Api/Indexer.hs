-- SPDX-FileCopyrightText: 2021 Serokell <https://serokell.io>
--
-- SPDX-License-Identifier: MPL-2.0

-- | The REST API v2 of the Algorand indexer.
--
-- See <https://developer.algorand.org/docs/reference/rest-apis/indexer/>
module Network.Algorand.Api.Indexer
  ( IndexerApi (..)
  , IdxAccountResponse (..)
  ) where

import Data.Aeson.TH (deriveJSON)
import GHC.Generics (Generic)
import Servant.API (Capture, Get, JSON, QueryParam, (:>))
import Servant.API.Generic ((:-))

import Data.Algorand.Address (Address)
import Data.Algorand.Round (Round)
import Network.Algorand.Api.Json (algorandTrainOptions)
import Network.Algorand.Api.Type (Account)

----------------
-- API
----------------

-- | Indexer API.
newtype IndexerApi route = IndexerApi
  { _accountIdx :: route
      :- "accounts"
      :> Capture "address" Address
      :> QueryParam "round" Round
      :> Get '[JSON] IdxAccountResponse
  } deriving stock (Generic)

----------------
-- Types
----------------

-- | Data sent in response to @/accounts@.
data IdxAccountResponse = IdxAccountResponse
  { iarAccount :: Account
  , iarCurrentRound :: Round
  } deriving stock (Generic, Show)

$(deriveJSON algorandTrainOptions 'IdxAccountResponse)
