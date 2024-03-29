-- SPDX-FileCopyrightText: 2021 Serokell <https://serokell.io>
--
-- SPDX-License-Identifier: MPL-2.0

-- | Communicate with indexer commands.
module Halgo.CLA.Command.Indexer
  ( indexerOpts
  ) where

import Control.Monad.Reader (asks)
import Fmt ((+|), (|+))
import Options.Applicative (Parser, command, help, hsubparser, info, long, metavar, optional,
                            progDesc, short, strOption)

import qualified Network.Algorand.Api as Api
import qualified Network.Algorand.Util as N

import Data.Algorand.Address (Address)
import Data.Algorand.Round (Round (..))
import Network.Algorand.Definitions (DefaultHost (ahIndexer), Host, getDefaultHost)

import Halgo.CLA.Argument (argAddress)
import Halgo.CLA.Option (optRound)
import Halgo.CLA.Type (MonadSubCommand, SubCommand, goNetwork)
import Halgo.IO (putJson, putTextLn)
import Halgo.Util (die, withIndexer)

optIndexerHost :: Parser (Maybe Host)
optIndexerHost = optional . strOption $ mconcat
  [ long "indexer-host"
  , short 'i'
  , metavar "INDEXER_HOST"
  , help "HOST of the indexer to connect to \
        \(default: AlgoExplorer indexer based on the chosen network)"
  ]

indexerOpts :: Parser SubCommand
indexerOpts = cmdIndexer <$> optIndexerHost <*> hsubparser (mconcat
  [ command "host"
    $ info (pure cmdIndexerHost)
    $ progDesc "Show the HOST of the indexer that will be used"

  , command "version"
    $ info (pure cmdIndexerVersion)
    $ progDesc "Query the version information of the indexer"

  , command "status"
    $ info (pure cmdIndexerStatus)
    $ progDesc "Show the status of the indexer that will be used"

  , command "fetch"
    $ info (hsubparser $ mconcat
      [ command "account"
        $ info
          (   cmdIndexerFetchAccount
          <$> argAddress "Account to fetch"
          <*> optional optRound
          )
        $ progDesc "Fetch information about an account"

      , command "block"
        $ info (cmdFetchBlock <$> optRound)
        $ progDesc "Retrieve block at given round"
      ])
    $ progDesc "Fetch data from the indexer"
  ])

cmdIndexer :: MonadSubCommand m => Maybe Host -> (Host -> m ()) -> m ()
cmdIndexer url sub = getIndexerHost url >>= sub
  where
    getIndexerHost :: MonadSubCommand m => Maybe Host -> m Host
    getIndexerHost (Just u) = pure u
    getIndexerHost Nothing = do
      network <- asks goNetwork
      case getDefaultHost network of
        Just host -> pure $ ahIndexer host
        Nothing -> die $
          "Unknown network `"+| show network |+"`. Please, provide --indexer-host."

-- | Display the URL that we will be using.
cmdIndexerHost :: MonadSubCommand m => Host -> m ()
cmdIndexerHost = putTextLn

-- | Get indexer version.
cmdIndexerVersion :: MonadSubCommand m => Host -> m ()
cmdIndexerVersion url = withIndexer url $ \(v, _) -> putJson v

-- | Get indexer status.
cmdIndexerStatus :: MonadSubCommand m => Host -> m ()
cmdIndexerStatus url = withIndexer url $ \(_, api) ->
  Api._health api >>= putJson

-- | Fetch information about an account.
cmdIndexerFetchAccount
  :: MonadSubCommand m
  => Address -> Maybe Round -> Host -> m ()
cmdIndexerFetchAccount addr rnd url = withIndexer url $ \(_, api) ->
  N.getAccountAtRound api addr rnd >>= \case
    Just d -> putJson d
    Nothing -> putJson $ "Account " <> show addr <> "is not found"

-- | Fetch information about a block.
cmdFetchBlock :: MonadSubCommand m => Round -> Host -> m ()
cmdFetchBlock rnd url = withIndexer url $ \(_, api) ->
  N.getBlockAtRound api rnd >>= \case
    Just d -> putJson d
    Nothing -> putJson $ "No block found for round " <> show (unRound rnd)
