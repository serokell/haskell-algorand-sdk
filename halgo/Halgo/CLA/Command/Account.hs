-- SPDX-FileCopyrightText: 2021 Serokell <https://serokell.io>
--
-- SPDX-License-Identifier: MPL-2.0

-- | Manage accounts commands.
module Halgo.CLA.Command.Account
  ( accountOpts
  , loadAccount
  ) where

import qualified Data.ByteString as BS
import qualified Data.Text.IO as TIO
import qualified System.IO.Error as IOE

import Control.Exception.Safe (handle, throwIO)
import Control.Monad ((>=>))
import Fmt ((+|), (|+))
import Options.Applicative (Parser, command, hsubparser, info, progDesc)
import UnliftIO (MonadIO, liftIO)
import UnliftIO.Directory (doesPathExist)
import UnliftIO.IO (IOMode (ReadMode, WriteMode), withFile)

import qualified Crypto.Algorand.Key as K
import qualified Data.Algorand.Address as A

import Crypto.Algorand.Key (SecretKey)

import Halgo.CLA.Argument (argSecretFile)
import Halgo.CLA.Type (MonadSubCommand, SubCommand)
import Halgo.IO (putTextLn)
import Halgo.Util (die)

accountOpts :: Parser SubCommand
accountOpts = hsubparser $ mconcat
  [ command "new"
    $ info (cmdAccNew <$> argSecretFile)
    $ progDesc "Create a new account"

  , command "show"
    $ info (cmdAccShow <$> argSecretFile)
    $ progDesc "Show account address"

  , command "export"
    $ info (cmdAccExport <$> argSecretFile)
    $ progDesc "Display SECRET key in the standard Algorand base64 representation"
  ]

-- | Generate a new account.
cmdAccNew :: MonadSubCommand m => FilePath -> m ()
cmdAccNew skFile = liftIO $ do
  doesPathExist skFile >>= \case
    True -> die $ "Not creating: "+|skFile|+" already exists."
    False -> do
      sk <- withFile skFile WriteMode $ \h -> do
        sk <- K.keypair
        TIO.hPutStr h (K.skToText sk)
        pure sk
      putTextLn (A.toText . A.fromPublicKey . K.toPublic $ sk)

-- | Display an account in base64.
cmdAccShow :: MonadSubCommand m => FilePath -> m ()
cmdAccShow = loadAccount >=> putTextLn . A.toText . A.fromPublicKey . K.toPublic

-- | Display an account in base64.
cmdAccExport :: MonadSubCommand m => FilePath -> m ()
cmdAccExport = loadAccount >=> putTextLn . K.skToText

-- | Helper that loads a secret key from the file (or crashes).
loadAccount :: MonadIO m => FilePath -> m SecretKey
loadAccount skFile = liftIO $ handle showError $
  withFile skFile ReadMode $ \h -> do
    skText <- BS.hGetLine h
    case K.skFromText skText of
      Nothing -> die "Invalid secret key (file corrupted?)."
      Just sk -> pure sk
  where
    showError e
      | IOE.isDoesNotExistError e = die $ "Account file does not exist: "+|skFile|+""
      | IOE.isPermissionError e = die $ "Permission denied. Cannot read: "+|skFile|+""
      | otherwise = throwIO e
