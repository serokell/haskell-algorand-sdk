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

import Options.Applicative (CommandFields, Mod, Parser, command, help, info, long, metavar,
                            progDesc, showDefaultWith, strOption, value)

import Halgo.CLA.Command.Account (accountOpts)
import Halgo.CLA.Command.Contract (contractOpts)
import Halgo.CLA.Command.Node (nodeOpts)
import Halgo.CLA.Command.Transaction (transactionOpts)
import Halgo.CLA.Type as Type

globalOpts :: Parser GlobalOptions
globalOpts = GlobalOptions <$> strOption (mconcat
  [ long "network"
  , metavar "GENESIS-ID"
  , help "Genesis ID of the Algorand network to work in"
  , value "testnet-v1.0"
  , showDefaultWith T.unpack
  ])

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
    $ progDesc "Communicate with algod"

  , command "txn"
    $ info transactionOpts
    $ progDesc "Work with transactions"
  ]
