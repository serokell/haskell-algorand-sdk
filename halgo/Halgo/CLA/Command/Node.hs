-- SPDX-FileCopyrightText: 2021 Serokell <https://serokell.io>
--
-- SPDX-License-Identifier: MPL-2.0

{-# OPTIONS_GHC -Wno-deprecations #-}

-- | Communicate with node commands.
module Halgo.CLA.Command.Node
  ( nodeOpts
  , cmdNode
  , optNodeHost
  ) where


import Control.Monad.Reader (asks)
import Data.Text (Text)
import Fmt (pretty, (+|), (|+))
import Options.Applicative (Parser, command, help, hsubparser, info, long, metavar, optional,
                            progDesc, short, strOption)

import qualified Network.Algorand.Api as Api
import qualified Network.Algorand.Util as N

import Network.Algorand.Definitions (DefaultHost (ahNode), Host, getDefaultHost)

import Halgo.CLA.Argument (argTxId)
import Halgo.CLA.Flag (flagJson)
import Halgo.CLA.Type (MonadSubCommand, SubCommand, goNetwork)
import Halgo.IO (putJson, putTextLn, readItemsB64, readItemsJson)
import Halgo.Util (die, withNode)

optNodeHost :: Parser (Maybe Host)
optNodeHost = optional . strOption $ mconcat
  [ long "node-host"
  , short 'n'
  , metavar "NODE_HOST"
  , help "HOST of the node to connect to \
        \(default: AlgoExplorer node based on the chosen network)"
  ]

nodeOpts :: Parser SubCommand
nodeOpts = cmdNode <$> optNodeHost <*> hsubparser (mconcat
  [ command "host"
    $ info (pure cmdNodeHost)
    $ progDesc "Show the HOST of the node that will be used"

  , command "version"
    $ info (pure cmdNodeVersion)
    $ progDesc "Query the version information of the node"

  , command "status"
    $ info (pure cmdNodeStatus)
    $ progDesc "Show the status of the node that will be used"

  , command "fetch"
    $ info (cmdNodeFetchTxn <$> argTxId)
    $ progDesc "Fetch a transaction in the pool"

  , command "send"
    $ info (cmdNodeSend <$> flagJson)
    $ progDesc "Send signed transactions (reads from stdin)"

  , command "txn-status"
    $ info (cmdNodeTxnStatus <$> argTxId)
    $ progDesc "Get the status of a transaction in the pool"
  ])

cmdNode :: MonadSubCommand m => Maybe Host -> (Host -> m ()) -> m ()
cmdNode url sub = getNodeHost url >>= sub
  where
    getNodeHost :: MonadSubCommand m => Maybe Host -> m Host
    getNodeHost (Just u) = pure u
    getNodeHost Nothing = do
      network <- asks goNetwork
      case getDefaultHost network of
        Just host -> pure $ ahNode host
        Nothing -> die $
          "Unknown network `"+| show network |+"`. Please, provide --node-host."

-- | Display the URL that we will be using.
cmdNodeHost :: MonadSubCommand m => Host -> m ()
cmdNodeHost = putTextLn

-- | Get node version.
cmdNodeVersion :: MonadSubCommand m => Host -> m ()
cmdNodeVersion url = withNode url $ \(v, _) -> putJson v

-- | Get node status.
cmdNodeStatus :: MonadSubCommand m => Host -> m ()
cmdNodeStatus url = withNode url $ \(_, api) ->
  Api._status api >>= putJson

-- | Fetch a transaction (from the pool).
cmdNodeFetchTxn :: MonadSubCommand m => Text -> Host -> m ()
cmdNodeFetchTxn txId url = withNode url $ \(_, api) ->
  Api._transactionsPending api txId >>= putJson . Api.tiTxn

-- | Send transactions.
cmdNodeSend :: MonadSubCommand m => Bool -> Host -> m ()
cmdNodeSend json url = do
    txns <- if json then readItemsJson else readItemsB64
    withNode url $ \(_, api) ->
      Api._transactions api txns >>= putTextLn . Api.trTxId

-- | Get txn status
cmdNodeTxnStatus :: MonadSubCommand m => Text -> Host -> m ()
cmdNodeTxnStatus txId url = withNode url $ \(_, api) ->
  Api._transactionsPending api txId >>= (\case
    N.Waiting -> putTextLn "Waiting"
    N.KickedOut reason -> die $ "Kicked out: "+|reason|+""
    N.Confirmed r -> putTextLn . pretty $ "Confirmed in round " <> show r
  ) . N.transactionStatus
