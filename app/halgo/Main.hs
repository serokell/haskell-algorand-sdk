-- SPDX-FileCopyrightText: 2021 Serokell <https://serokell.io>
--
-- SPDX-License-Identifier: MPL-2.0

-- | The @halgo@ CLI.
module Main
  ( main
  ) where


import Options.Applicative

import Control.Exception.Safe (handle, throwIO)
import Control.Monad ((>=>))
import Control.Monad.Reader (MonadReader, ReaderT, runReaderT)
import qualified Data.ByteString as BS
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Main.Utf8 (withUtf8)
import System.Exit (die)
import qualified System.IO.Error as IOE
import UnliftIO (MonadUnliftIO, liftIO)
import UnliftIO.Directory (doesPathExist)
import UnliftIO.IO (IOMode (ReadMode, WriteMode), withFile)

import Crypto.Algorand.Signature (SecretKey)
import qualified Crypto.Algorand.Signature as S
import qualified Data.Algorand.Address as A


-- | CLI options applicable to all commands.
data GlobalOptions = GlobalOptions
  { goNetwork :: Text
  }

type Subcommand = ReaderT GlobalOptions IO ()
type MonadSubcommand m = (MonadUnliftIO m, MonadReader GlobalOptions m)


opts :: Parser (GlobalOptions, Subcommand)
opts = (,) <$> optsGlobal <*> hsubparser optsCommand
  where
    optsGlobal = GlobalOptions
      <$> strOption
            ( long "network"
           <> metavar "GENESIS-ID"
           <> help "Genesis ID of the Algorand network to work in"
           <> value "testnet-1.0"
           <> showDefaultWith T.unpack
            )

    optsCommand = mconcat
      [ command "acc" $ info
          accountOpts
          (progDesc "Manage accounts (public and secret keys)")
      ]

-- | Main.
main :: IO ()
main = withUtf8 $ do
  let p = prefs $ mconcat [disambiguate, showHelpOnEmpty, showHelpOnError]
  let infoMod = progDesc "A CLI tool for interacting with the Algorand blockchain"
  (globalOptions, act) <- customExecParser p $ info (opts <**> helper) infoMod
  runReaderT act globalOptions



accountOpts :: Parser Subcommand
accountOpts = hsubparser $ mconcat
    [ command "new" $ info
        (cmdAccNew <$> skFile)
        (progDesc "Create a new account")
    , command "show" $ info
        (cmdAccShow <$> skFile)
        (progDesc "Show account address")
    , command "export" $ info
        (cmdAccExport <$> skFile)
        (progDesc "Display SECRET key in the standard Algorand base64 representation")
    ]
  where
    skFile :: Parser FilePath
    skFile = strArgument
      ( metavar "<keyfile>"
     <> help "Path to a file of the account."
     <> action "file"
      )

-- | Generate a new account.
cmdAccNew :: MonadSubcommand m => FilePath -> m ()
cmdAccNew skFile = liftIO $ do
  doesPathExist skFile >>= \case
    True -> die $ "Not creating: " <> skFile <> " already exists."
    False -> do
      sk <- withFile skFile WriteMode $ \h -> do
        sk <- S.keypair
        T.hPutStr h (S.skToText sk)
        pure sk
      T.putStrLn (A.toText . A.fromPublicKey . S.toPublic $ sk)

-- | Helper that loads a secret key from the file (or crashes).
loadAccount :: MonadSubcommand m => FilePath -> m SecretKey
loadAccount skFile = liftIO $ handle showError $
    withFile skFile ReadMode $ \h -> do
      skText <- BS.hGetLine h
      case S.skFromText skText of
        Nothing -> die "Invalid secret key (file corrupted?)."
        Just sk -> pure sk
  where
    showError e
      | IOE.isDoesNotExistError e = die $ "Account file does not exist: " <> skFile
      | IOE.isPermissionError e = die $ "Permission denied. Cannot read: " <> skFile
      | otherwise = throwIO e

-- | Display an account in base64.
cmdAccShow :: MonadSubcommand m => FilePath -> m ()
cmdAccShow = loadAccount >=> liftIO . T.putStrLn . A.toText . A.fromPublicKey . S.toPublic

-- | Display an account in base64.
cmdAccExport :: MonadSubcommand m => FilePath -> m ()
cmdAccExport = loadAccount >=> liftIO . T.putStrLn . S.skToText
