-- SPDX-FileCopyrightText: 2021 Serokell <https://serokell.io>
--
-- SPDX-License-Identifier: MPL-2.0

-- | Communicate with node commands.
module Halgo.CLA.Command.Node
  ( nodeOpts
  , cmdNode
  , optNodeUrl
  ) where


import Control.Monad.Reader (asks)
import Data.Text (Text)
import Fmt (pretty, (+|), (|+))
import Options.Applicative (Parser, auto, command, help, hsubparser, info, long, metavar, option,
                            optional, progDesc, short, strOption)

import qualified Data.Algorand.Address as A
import qualified Data.Algorand.Block as B
import qualified Data.Algorand.Transaction.Signed as TS
import qualified Network.Algorand.Node.Api as Api
import qualified Network.Algorand.Node.Util as N

import Network.Algorand.Node (NodeUrl)

import Halgo.CLA.Argument (argAddress, argTxId)
import Halgo.CLA.Flag (flagJson)
import Halgo.CLA.Type (MonadSubCommand, SubCommand, goNetwork)
import Halgo.IO (putJson, putTextLn, readItemsB64, readItemsJson)
import Halgo.Util (die, withNode)

optNodeUrl :: Parser (Maybe NodeUrl)
optNodeUrl = optional . strOption $ mconcat
  [ long "url"
  , short 'u'
  , metavar "NODE_URL"
  , help "URL of the node to connect to (default: AlgoExplorer node based on the chosen network)"
  ]

optRound :: Parser B.Round
optRound = B.Round <$> option auto (mconcat
  [ long "round"
  , metavar "<round number>"
  , help "Round number of Algorand blockchain"
  ])

nodeOpts :: Parser SubCommand
nodeOpts = cmdNode <$> optNodeUrl <*> hsubparser (mconcat
  [ command "url"
    $ info (pure cmdNodeUrl)
    $ progDesc "Show the URL of the node that will be used"

  , command "version"
    $ info (pure cmdNodeVersion)
    $ progDesc "Query the version information of the node"

  , command "status"
    $ info (pure cmdNodeStatus)
    $ progDesc "Show the status of the node that will be used"

  , command "block"
    $ info (cmdPrintBlock <$> optRound)
    $ progDesc "Retrieve block"

  , command "fetch"
    $ info (hsubparser $ mconcat
      [ command "acc"
        $ info (cmdNodeFetchAccount <$> argAddress "Account to fetch")
        $ progDesc "Fetch information about an account"

      , command "txn"
        $ info (cmdNodeFetchTxn <$> argTxId)
        $ progDesc "Fetch a transaction in the pool"
      ])
    $ progDesc "Fetch something from the node"

  , command "send"
    $ info (cmdNodeSend <$> flagJson)
    $ progDesc "Send signed transactions (reads from stdin)"

  , command "txn-status"
    $ info (cmdNodeTxnStatus <$> argTxId)
    $ progDesc "Get the status of a transaction in the pool"
  ])

cmdNode :: MonadSubCommand m => Maybe NodeUrl -> (NodeUrl -> m ()) -> m ()
cmdNode url sub = getNodeUrl url >>= sub
  where
    getNodeUrl :: MonadSubCommand m => Maybe NodeUrl -> m NodeUrl
    getNodeUrl (Just u) = pure u
    getNodeUrl Nothing = asks goNetwork >>= \case
      "mainnet-v1.0" -> pure "https://api.algoexplorer.io/"
      "testnet-v1.0" -> pure "https://api.testnet.algoexplorer.io/"
      "betanet-v1.0" -> pure "https://api.betanet.algoexplorer.io/"
      net -> die $ "Unknown network `"+|net|+"`. Please, provide --url."

-- | Display the URL that we will be using.
cmdNodeUrl :: MonadSubCommand m => NodeUrl -> m ()
cmdNodeUrl = putTextLn

-- | Get node version.
cmdNodeVersion :: MonadSubCommand m => NodeUrl -> m ()
cmdNodeVersion url = withNode url $ \(v, _) -> putJson v

-- | Get node status.
cmdNodeStatus :: MonadSubCommand m => NodeUrl -> m ()
cmdNodeStatus url = withNode url $ \(_, api) ->
  Api._status api >>= putJson

-- | Print block info.
cmdPrintBlock :: MonadSubCommand m => B.Round -> NodeUrl -> m ()
cmdPrintBlock rnd url = withNode url $ \(_, api) -> do
  mBlock <- N.getBlock api rnd
  case mBlock of
    Just block -> do
      let txs = TS.getUnverifiedTransaction . TS.toSignedTransaction
                True -- false should be used only for some old protocol versions
                (B.bGenesisHash block)
                (B.bGenesisId block)
            <$> B.bTransactions block
      putTextLn $ "Retrieved " +| (if null txs then "empty " else "" :: Text)
        |+ "block for round " +| B.unRound (B.bRound block)
        |+ " created at " +| B.bTimestamp block
        |+ (if null txs then "" else " with txs:")
      mapM_ putJson txs
    _ -> putTextLn "No block found"

-- | Fetch information about an account.
cmdNodeFetchAccount :: MonadSubCommand m => A.Address -> NodeUrl -> m ()
cmdNodeFetchAccount addr url = withNode url $ \(_, api) ->
  Api._account api addr >>= putJson

-- | Fetch a transaction (from the pool).
cmdNodeFetchTxn :: MonadSubCommand m => Text -> NodeUrl -> m ()
cmdNodeFetchTxn txId url = withNode url $ \(_, api) ->
  Api._transactionsPending api txId >>= putJson . Api.tiTxn

-- | Send transactions.
cmdNodeSend :: MonadSubCommand m => Bool -> NodeUrl -> m ()
cmdNodeSend json url = do
    txns <- if json then readItemsJson else readItemsB64
    withNode url $ \(_, api) ->
      Api._transactions api txns >>= putTextLn . Api.trTxId

-- | Get txn status
cmdNodeTxnStatus :: MonadSubCommand m => Text -> NodeUrl -> m ()
cmdNodeTxnStatus txId url = withNode url $ \(_, api) ->
  N.transactionStatus <$> Api._transactionsPending api txId >>= \case
    N.Waiting -> putTextLn "Waiting"
    N.KickedOut reason -> die $ "Kicked out: "+|reason|+""
    N.Confirmed r -> putTextLn . pretty $ "Confirmed in round " <> show r
