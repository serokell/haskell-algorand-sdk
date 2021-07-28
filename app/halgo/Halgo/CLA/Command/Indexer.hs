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

import Network.Algorand.Definitions (DefaultHost (ahIndexer), Host, getDefaultHost)

import Halgo.CLA.Type (MonadSubCommand, SubCommand, goNetwork)
import Halgo.IO (putTextLn)
import Halgo.Util (die)

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
