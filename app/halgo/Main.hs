-- SPDX-FileCopyrightText: 2021 Serokell <https://serokell.io>
--
-- SPDX-License-Identifier: MPL-2.0

-- | The @halgo@ CLI.
module Main
  ( main
  ) where


import Options.Applicative

import Control.Exception.Safe (MonadThrow, handle, throwIO)
import Control.Monad ((>=>), when)
import Control.Monad.Reader (MonadReader, ReaderT, asks, runReaderT)
import Data.Aeson (ToJSON)
import Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.ByteString as BS
import Data.ByteString.Base64 (decodeBase64)
import qualified Data.ByteString.Lazy as BSL
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)
import qualified Data.Text.IO as T
import Main.Utf8 (withUtf8)
import Servant.Client.Generic (AsClientT)
import qualified System.Exit
import qualified System.IO.Error as IOE
import UnliftIO (MonadIO, MonadUnliftIO, liftIO)
import UnliftIO.Directory (doesPathExist)
import UnliftIO.IO (IOMode (ReadMode, WriteMode), withFile)

import qualified Data.Algorand.MessagePack as MP
import Crypto.Algorand.Signature (SecretKey)
import qualified Crypto.Algorand.Signature as S
import qualified Data.Algorand.Address as A
import qualified Data.Algorand.Transaction as T
import Network.Algorand.Node (NodeUrl, connect)
import Network.Algorand.Node.Api (ApiV2)
import qualified Network.Algorand.Node.Api as Api


-- | CLI options applicable to all commands.
data GlobalOptions = GlobalOptions
  { goNetwork :: Text
  }

type Subcommand = ReaderT GlobalOptions IO ()
type MonadSubcommand m = (MonadUnliftIO m, MonadThrow m, MonadReader GlobalOptions m)


opts :: Parser (GlobalOptions, Subcommand)
opts = (,) <$> optsGlobal <*> hsubparser optsCommand
  where
    optsGlobal = GlobalOptions
      <$> strOption
            ( long "network"
           <> metavar "GENESIS-ID"
           <> help "Genesis ID of the Algorand network to work in"
           <> value "testnet-v1.0"
           <> showDefaultWith T.unpack
            )

    optsCommand = mconcat
      [ command "acc" $ info
          accountOpts
          (progDesc "Manage accounts (public and secret keys)")
      , command "txn" $ info
          txnOpts
          (progDesc "Work with transactions")
      , command "node" $ info
          nodeOpts
          (progDesc "Communicate with algod")
      ]

-- | Main.
main :: IO ()
main = withUtf8 $ do
  let p = prefs $ mconcat [disambiguate, showHelpOnEmpty, showHelpOnError]
  let infoMod = progDesc "A CLI tool for interacting with the Algorand blockchain"
  (globalOptions, act) <- customExecParser p $ info (opts <**> helper) infoMod
  runReaderT act globalOptions

die :: MonadIO m => String -> m a
die = liftIO . System.Exit.die

putTextLn :: MonadIO m => Text -> m ()
putTextLn = liftIO . T.putStrLn

printJson :: (MonadIO m, ToJSON a) => a -> m ()
printJson = putTextLn . decodeUtf8 . BSL.toStrict . encodePretty


{-
 - halgo account
 -}

accountOpts :: Parser Subcommand
accountOpts = hsubparser $ mconcat
    [ command "new" $ info
        (cmdAccNew <$> argSkFile)
        (progDesc "Create a new account")
    , command "show" $ info
        (cmdAccShow <$> argSkFile)
        (progDesc "Show account address")
    , command "export" $ info
        (cmdAccExport <$> argSkFile)
        (progDesc "Display SECRET key in the standard Algorand base64 representation")
    ]

argSkFile :: Parser FilePath
argSkFile = strArgument $ mconcat
  [ metavar "<keyfile>"
  , help "Path to a file of the account"
  , action "file"
  ]

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
      putTextLn (A.toText . A.fromPublicKey . S.toPublic $ sk)

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
cmdAccShow = loadAccount >=> putTextLn . A.toText . A.fromPublicKey . S.toPublic
-- | Display an account in base64.
cmdAccExport :: MonadSubcommand m => FilePath -> m ()
cmdAccExport = loadAccount >=> putTextLn . S.skToText


{-
 - halgo txn
 -}

txnOpts :: Parser Subcommand
txnOpts = hsubparser $ mconcat
    [ command "show" $ info
        (cmdTxnShow <$> verifyFlag)
        (progDesc "Decode and display a signed transaction (reads from stdin)")
    , command "show-unsigned" $ info
        (pure cmdTxnShowUnsigned)
        (progDesc "Decode and display an unsigned transaction (reads from stdin)")
    ]
  where
    verifyFlag = flag True False $ mconcat
      [ long "no-verify"
      , short 'n'
      , help "Do not verify the signature of the transaction"
      ]

-- | Show a transaction.
cmdTxnShow :: MonadSubcommand m => Bool -> m ()
cmdTxnShow verify = liftIO $ do
  b64 <- BS.getLine
  case decodeBase64 b64 of
    Left err -> die $ T.unpack err
    Right bs -> do
      MP.Canonical stxn <- MP.unpack (BSL.fromStrict bs)
      when verify $
        case T.verifyTransaction stxn of
          Nothing -> die "Invalid signature. Run with --no-verify if you still want to see it."
          Just _txn -> pure ()
      printJson stxn

-- | Show an unsigne transaction.
cmdTxnShowUnsigned :: MonadSubcommand m => m ()
cmdTxnShowUnsigned = liftIO $ do
  b64 <- BS.getLine
  case decodeBase64 b64 of
    Left err -> die $ T.unpack err
    Right bs -> do
      MP.Canonical txn <- MP.unpack (BSL.fromStrict bs)
      printJson (txn :: T.Transaction)


{-
 - halgo node
 -}

argAddress :: Parser A.Address
argAddress = argument reader $ mconcat
    [ metavar "<address>"
    , help "Address of an account"
    ]
  where
    reader = eitherReader $ \s -> case A.fromText (T.pack s) of
      Nothing -> Left "Malformed address."
      Just a -> Right a

nodeOpts :: Parser Subcommand
nodeOpts = cmdNode <$> nodeUrl <*> sub
  where
    sub = hsubparser $ mconcat
      [ command "url" $ info
          (pure cmdNodeUrl)
          (progDesc "Show the URL of the node that will be used")
      , command "version" $ info
          (pure cmdNodeVersion)
          (progDesc "Query the version information of the node")
      , command "fetch" $ info
          (cmdNodeFetch <$> fetchArg)
          (progDesc "Fetch information about an account")
      ]

    nodeUrl :: Parser (Maybe NodeUrl)
    nodeUrl
      =   Just <$> (strOption $ mconcat
            [ long "url"
            , short 'u'
            , metavar "NODE_URL"
            , help "URL of the node to connect to (default: AlgoExplorer node based on the chosen network)"
            ])
      <|> pure Nothing

    fetchArg = NodeFetchAddress <$> argAddress

cmdNode :: MonadSubcommand m => Maybe NodeUrl -> (NodeUrl -> m ()) -> m ()
cmdNode murl sub = getNodeUrl murl >>= sub
  where
    getNodeUrl :: MonadSubcommand m => Maybe NodeUrl -> m NodeUrl
    getNodeUrl (Just u) = pure u
    getNodeUrl Nothing = asks goNetwork >>= \case
      "mainnet-v1.0" -> pure "https://api.algoexplorer.io/"
      "testnet-v1.0" -> pure "https://api.testnet.algoexplorer.io/"
      "betanet-v1.0" -> pure "https://api.betanet.algoexplorer.io/"
      net -> die $ "Unknown network `" <> T.unpack net <> "`. Please, provide --url."

-- | Display the URL that we will be using.
cmdNodeUrl :: MonadSubcommand m => NodeUrl -> m ()
cmdNodeUrl url = putTextLn url

-- | Connect to a node and check that its network is what we expect.
withNode
  :: MonadSubcommand m
  => NodeUrl
  -> ((Api.Version, ApiV2 (AsClientT m)) -> m a)
  -> m a
withNode url act = do
  net <- asks goNetwork
  connect url net >>= act

cmdNodeVersion :: MonadSubcommand m => NodeUrl -> m ()
cmdNodeVersion url = withNode url $ \(v, _) -> printJson v


data NodeFetchArgument
  = NodeFetchAddress A.Address

cmdNodeFetch :: MonadSubcommand m => NodeFetchArgument -> NodeUrl -> m ()
cmdNodeFetch (NodeFetchAddress addr) url = withNode url $ \(_, api) ->
  Api._account api addr >>= printJson
