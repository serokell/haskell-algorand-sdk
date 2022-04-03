-- SPDX-FileCopyrightText: 2021 Serokell <https://serokell.io>
--
-- SPDX-License-Identifier: MPL-2.0

-- | Definitions to work with Algo, grouped in this module for convenience.
module Network.Algorand.Definitions
  ( Network (.., MainnetV1, TestnetV1, BetanetV1)
  , NetworkError (..)
  , InvalidNetwork (..)
  , isKnownNetwork
  , knownNetworkParser

  , Host
  , DefaultHost (..)
  , getDefaultHost
  ) where

import qualified Data.Map.Strict as Map
import qualified Data.Text as T

import Control.Exception.Safe (Exception (displayException))
import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.Map.Strict (Map)
import Data.Text (Text)
import GHC.Exts (IsString)

-- | Supported networks.
--
-- Also referred as \"genesis id\".
newtype Network = Network Text
  deriving (Eq, Ord, IsString)

pattern MainnetV1, TestnetV1, BetanetV1 :: Network
pattern MainnetV1 = Network "mainnet-v1.0"
pattern TestnetV1 = Network "testnet-v1.0"
pattern BetanetV1 = Network "betanet-v1.0"

instance Show Network where
  show (Network t) = T.unpack t

instance ToJSON Network where
  toJSON (Network t) = toJSON t

instance FromJSON Network where
  parseJSON o = Network <$> parseJSON o

-- | URL of the node or indexer to connect to.
type Host = Text

-- | Error thrown if 'connect' fails.
data NetworkError = WrongNetwork
  { wnExpected :: Network
  , wnActual :: Network
  }

instance Show NetworkError where
  show WrongNetwork{wnExpected, wnActual} = mconcat
    [ "Bad host: the node is connected to `" <> show wnActual <> "`, "
    , "we expected `" <> show wnExpected <> "`."
    ]

instance Exception NetworkError

newtype InvalidNetwork = InvalidNetwork String
  deriving stock (Show, Eq)

instance Exception InvalidNetwork where
  displayException (InvalidNetwork e) = "Invalid network: " <> e

isKnownNetwork :: Network -> Bool
isKnownNetwork =
  flip elem
  [ MainnetV1, TestnetV1, BetanetV1 ]

knownNetworkParser :: Text -> Either InvalidNetwork Network
knownNetworkParser t =
  let res = Network t
  in if isKnownNetwork res
    then Right res
    else Left . InvalidNetwork . T.unpack $ "Unknown network name: " <> t

data DefaultHost = DefaultHost
  { ahNode :: Host
  , ahIndexer :: Host
  } deriving Show

defaultHost :: Map Network DefaultHost
defaultHost = Map.fromList
  [ ( MainnetV1
    , DefaultHost
      { ahNode = "https://api.algoexplorer.io/"
      , ahIndexer = "https://indexer.algoexplorerapi.io/"
      }
    )
  , ( TestnetV1
    , DefaultHost
      { ahNode = "https://api.testnet.algoexplorer.io/"
      , ahIndexer = "https://indexer.testnet.algoexplorerapi.io/"
      }
    )
  , ( BetanetV1
    , DefaultHost
      { ahNode = "https://api.betanet.algoexplorer.io/"
      , ahIndexer = "https://indexer.betanet.algoexplorerapi.io/"
      }
    )
  ]

-- | Get default host by `Network`
getDefaultHost :: Network -> Maybe DefaultHost
getDefaultHost network = Map.lookup network defaultHost
