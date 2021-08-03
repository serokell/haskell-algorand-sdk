-- SPDX-FileCopyrightText: 2021 Serokell <https://serokell.io>
--
-- SPDX-License-Identifier: MPL-2.0

-- | Common options for commands
module Halgo.CLA.Option
  ( optRound
  ) where

import Options.Applicative (Parser, auto, help, long, metavar, option)

import Data.Algorand.Block (Round (..))

optRound :: Parser Round
optRound = Round <$> option auto (mconcat
  [ long "round"
  , metavar "<round number>"
  , help "Round number of Algorand blockchain"
  ])
