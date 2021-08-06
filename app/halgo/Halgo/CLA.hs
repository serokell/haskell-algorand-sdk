-- SPDX-FileCopyrightText: 2021 Serokell <https://serokell.io>
--
-- SPDX-License-Identifier: MPL-2.0

-- | All available commands
module Halgo.CLA
  ( globalOpts
  , commands

  , module Type
  ) where

import qualified Data.Text as T

import Options.Applicative (CommandFields, Mod, Parser, command, eitherReader, help, info, long,
                            metavar, option, progDesc, showDefault, value)

import Network.Algorand.Definitions (InvalidNetwork (..), Network (..), networkParser)

import Halgo.CLA.Command.Account (accountOpts)
import Halgo.CLA.Command.Contract (contractOpts)
import Halgo.CLA.Command.Indexer (indexerOpts)
import Halgo.CLA.Command.Node (nodeOpts)
import Halgo.CLA.Command.Transaction (transactionOpts)
import Halgo.CLA.Type as Type

globalOpts :: Parser GlobalOptions
globalOpts = GlobalOptions <$> option (eitherReader parser) (mconcat
  [ long "network"
  , metavar "GENESIS-ID"
  , help "Genesis ID of the Algorand network to work in"
  , value TestnetV1
  , showDefault
  ])
  where
    parser a = case networkParser $ T.pack a of
      Right v -> Right v
      Left (InvalidNetwork e) -> Left e

commands :: Mod CommandFields SubCommand
commands = mconcat
  [ command "acc"
    $ info accountOpts
    $ progDesc "Manage accounts (public and secret keys)"

  , command "contract"
    $ info contractOpts
    $ progDesc "Work with TEAL programs"

  , command "node"
    $ info nodeOpts
    $ progDesc "Communicate with algo node"

  , command "indexer"
    $ info indexerOpts
    $ progDesc "Communicate with algo indexer"

  , command "txn"
    $ info transactionOpts
    $ progDesc "Work with transactions"
  ]
