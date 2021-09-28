-- SPDX-FileCopyrightText: 2021 Serokell <https://serokell.io>
--
-- SPDX-License-Identifier: MPL-2.0

-- | Type definitions for CLA
module Halgo.CLA.Type
  ( GlobalOptions(..)
  , SubCommand
  , MonadSubCommand
  ) where

import Control.Exception.Safe (MonadCatch)
import Control.Monad.Reader (MonadReader, ReaderT)
import UnliftIO (MonadUnliftIO)

import Network.Algorand.Definitions (Network)

-- | CLI options applicable to all commands.
newtype GlobalOptions = GlobalOptions
  { goNetwork :: Network
  }

type SubCommand = ReaderT GlobalOptions IO ()

type MonadSubCommand m =
  ( MonadUnliftIO m
  , MonadCatch m
  , MonadReader GlobalOptions m
  )
