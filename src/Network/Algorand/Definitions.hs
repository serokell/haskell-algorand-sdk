-- SPDX-FileCopyrightText: 2021 Serokell <https://serokell.io>
--
-- SPDX-License-Identifier: MPL-2.0

-- | Definitions to work with Algo, grouped in this module for convenience.
module Network.Algorand.Definitions
  ( Network (..)
  , NetworkError (..)
  , InvalidNetwork (..)
  , networkParser

  , Host
  , DefaultHost (..)
  , getDefaultHost
  ) where

import qualified Data.Map.Strict as Map
import qualified Data.Text as T

import Control.Exception.Safe (Exception (displayException))
import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.Functor ((<&>))
import Data.Map.Strict (Map)
import Data.Text (Text)

-- | Supported networks.
data Network
  = MainnetV1
  | TestnetV1
  | BetanetV1
  deriving (Eq, Ord)

-- | Mapping from `Network` to it's name
networkToName :: Network -> Text
networkToName = \case
  MainnetV1 -> "mainnet-v1.0"
  TestnetV1 -> "testnet-v1.0"
  BetanetV1 -> "betanet-v1.0"

networkParser :: Text -> Either InvalidNetwork Network
networkParser = \case
  "mainnet-v1.0" -> Right MainnetV1
  "testnet-v1.0" -> Right TestnetV1
  "betanet-v1.0" -> Right BetanetV1
  x -> Left . InvalidNetwork . T.unpack $ "Unmapped network name: " <> x

instance Show Network where
  show = T.unpack . networkToName

instance ToJSON Network where
  toJSON = toJSON . networkToName

instance FromJSON Network where
  parseJSON o = do
    value <- parseJSON o <&> networkParser
    case value of
      Right v -> pure v
      Left (InvalidNetwork e) -> fail e

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
