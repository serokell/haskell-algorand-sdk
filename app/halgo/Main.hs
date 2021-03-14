-- SPDX-FileCopyrightText: 2021 Serokell <https://serokell.io>
--
-- SPDX-License-Identifier: MPL-2.0

-- | The @halgo@ CLI.
module Main
  ( main
  ) where


import Options.Applicative

import Control.Exception.Safe (MonadCatch, handle, throwIO)
import Control.Monad ((>=>), forM_, when)
import Control.Monad.Reader (MonadReader, ReaderT, asks, runReaderT)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Word (Word64)
import Fmt ((+|), (|+), pretty)
import Main.Utf8 (withUtf8)
import Servant.Client.Generic (AsClientT)
import qualified System.IO.Error as IOE
import UnliftIO (MonadIO, MonadUnliftIO, liftIO)
import UnliftIO.Directory (doesPathExist)
import UnliftIO.IO (IOMode (ReadMode, WriteMode), withFile)

import qualified Data.Algorand.MessagePack as MP
import Crypto.Algorand.Signature (SecretKey)
import qualified Crypto.Algorand.Signature as S
import qualified Data.Algorand.Address as A
import qualified Data.Algorand.Amount as A
import qualified Data.Algorand.Transaction as T
import qualified Data.Algorand.Transaction.Build as T
import Network.Algorand.Node (NodeUrl, connect)
import Network.Algorand.Node.Api (ApiV2)
import qualified Network.Algorand.Node.Api as Api
import qualified Network.Algorand.Node.Util as N

import Halgo.IO (putItemsB64, putItemsJson, putJson, putTextLn, readItemsB64, readItemsJson, readBytesB64)
import Halgo.Util (die, handleApiError)


-- | CLI options applicable to all commands.
data GlobalOptions = GlobalOptions
  { goNetwork :: Text
  }

type Subcommand = ReaderT GlobalOptions IO ()
type MonadSubcommand m = (MonadUnliftIO m, MonadCatch m, MonadReader GlobalOptions m)


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
    True -> die $ "Not creating: "+|skFile|+" already exists."
    False -> do
      sk <- withFile skFile WriteMode $ \h -> do
        sk <- S.keypair
        T.hPutStr h (S.skToText sk)
        pure sk
      putTextLn (A.toText . A.fromPublicKey . S.toPublic $ sk)

-- | Helper that loads a secret key from the file (or crashes).
loadAccount :: MonadIO m => FilePath -> m SecretKey
loadAccount skFile = liftIO $ handle showError $
    withFile skFile ReadMode $ \h -> do
      skText <- BS.hGetLine h
      case S.skFromText skText of
        Nothing -> die "Invalid secret key (file corrupted?)."
        Just sk -> pure sk
  where
    showError e
      | IOE.isDoesNotExistError e = die $ "Account file does not exist: "+|skFile|+""
      | IOE.isPermissionError e = die $ "Permission denied. Cannot read: "+|skFile|+""
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

flagJson :: Parser Bool
flagJson = flag False True $ mconcat
  [ long "json"
  , short 'j'
  , help "Read transactions as JSON instead of default base64"
  ]

flagB64 :: Parser Bool
flagB64 = flag False True $ mconcat
  [ long "base64"
  , short 'b'
  , help "Print transactions as base64 instead of JSON"
  ]

argAmount :: Parser A.Microalgos
argAmount = argument auto (metavar "<amount>" <> help "Amount in microalgos")

txnOpts :: Parser Subcommand
txnOpts = hsubparser $ mconcat
    [ command "show" $ info
        (cmdTxnShow <$> flagVerify <*> flagJson <*> flagB64)
        (progDesc "Decode from base64 and display signed transactions (reads from stdin)")
    , command "show-unsigned" $ info
        (cmdTxnShowUnsigned <$> flagJson <*> flagB64)
        (progDesc "Decode from base64 and display unsigned transactions (reads from stdin)")
    , command "sign" $ info
        (cmdTxnSign <$> flagJson <*> argSkFile)
        (progDesc "Sign one or multiple transactions (reads from stdin)")
    , command "id" $ info
        (cmdTxnId <$> flagJson)
        (progDesc "Calculate transaction ID")
    , command "new" $ info
        (cmdNode <$> optNodeUrl <*> hsubparser new)
        (progDesc "Create a new transaction")
    ]
  where
    flagVerify = flag True False $ mconcat
      [ long "no-verify"
      , short 'n'
      , help "Do not verify the signature of the transaction"
      ]

    argAssetIndex :: Parser T.AssetIndex
    argAssetIndex = argument auto (metavar "<asset>" <> help "Index of the asset")

    argAssetAmount :: Parser Word64
    argAssetAmount = argument auto (metavar "<amount>" <> help "Amount of asset")

    new = mconcat $
      [ command "pay" $ info
          (cmdTxnNewPay <$> argAddress "Receiver" <*> argAmount)
          (progDesc "Create a new Payment transaction")
      , command "axfr" $ info
          (cmdTxnNewAxfr <$> argAssetIndex <*> argAddress "Receiver" <*> argAssetAmount)
          (progDesc "Create a new AssetTransfer transaction")
      ]


-- | Show signed transactions.
cmdTxnShow :: MonadSubcommand m => Bool -> Bool -> Bool -> m ()
cmdTxnShow verify json base64 = do
  stxns <- if json then readItemsJson else readItemsB64
  when verify $
    forM_ stxns $ \stxn -> case T.verifyTransaction stxn of
      Nothing -> die "Invalid signature. Run with --no-verify if you still want to see it."
      Just _txn -> pure ()
  (if base64 then putItemsB64 else putItemsJson) stxns

-- | Show unsigned transactions.
cmdTxnShowUnsigned :: MonadSubcommand m => Bool -> Bool -> m ()
cmdTxnShowUnsigned json base64 =
  (if json then readItemsJson else readItemsB64 @T.Transaction)
  >>= if base64 then putItemsB64 else putItemsJson

-- | Sign transactions.
cmdTxnSign :: MonadSubcommand m => Bool -> FilePath -> m ()
cmdTxnSign json skFile = do
  sk <- loadAccount skFile
  txns <- if json then readItemsJson else readItemsB64
  let txns' = map (\txn -> txn { T.tSender = A.fromPublicKey $ S.toPublic sk }) txns
  let signed = map (T.signTransaction sk) txns'
  putItemsB64 signed

-- | Transaction ID.
cmdTxnId :: MonadSubcommand m => Bool -> m ()
cmdTxnId json = do
  txns <- if json then readItemsJson else readItemsB64
  mapM_ (putTextLn . T.transactionId) txns

cmdTxnNewPay :: MonadSubcommand m => A.Address -> A.Microalgos -> NodeUrl -> m ()
cmdTxnNewPay to amnt url = withNode url $ \(_, api) -> do
  params <- Api._transactionsParams api
  let payment = T.PaymentTransaction to amnt Nothing
  let txn = T.buildTransaction params A.zero payment
  putItemsB64 [txn]

cmdTxnNewAxfr :: MonadSubcommand m => T.AssetIndex -> A.Address -> Word64 -> NodeUrl -> m ()
cmdTxnNewAxfr index to amnt url = withNode url $ \(_, api) -> do
  params <- Api._transactionsParams api
  let payment = T.AssetTransferTransaction index amnt Nothing to Nothing
  let txn = T.buildTransaction params A.zero payment
  putItemsB64 [txn]


{-
 - halgo node
 -}

argAddress :: String -> Parser A.Address
argAddress helpText = argument reader $ mconcat
    [ metavar "<address>"
    , help helpText
    ]
  where
    reader = eitherReader $ \s -> case A.fromText (T.pack s) of
      Nothing -> Left "Malformed address."
      Just a -> Right a

optNodeUrl :: Parser (Maybe NodeUrl)
optNodeUrl
  =   Just <$> (strOption $ mconcat
        [ long "url"
        , short 'u'
        , metavar "NODE_URL"
        , help "URL of the node to connect to (default: AlgoExplorer node based on the chosen network)"
        ])
  <|> pure Nothing

nodeOpts :: Parser Subcommand
nodeOpts = cmdNode <$> optNodeUrl <*> sub
  where
    sub = hsubparser $ mconcat
      [ command "url" $ info
          (pure cmdNodeUrl)
          (progDesc "Show the URL of the node that will be used")
      , command "version" $ info
          (pure cmdNodeVersion)
          (progDesc "Query the version information of the node")
      , command "fetch" $ info
          (hsubparser $ mconcat
            [ command "acc" $ info
                (cmdNodeFetchAccount <$> argAddress "Account to fetch")
                (progDesc "Fetch information about an account")
            , command "txn" $ info
                (cmdNodeFetchTxn <$> argTxId)
                (progDesc "Fetch a transaction in the pool")
            ])
          (progDesc "Fetch something from the node")
      , command "send" $ info
          (cmdNodeSend <$> flagJson)
          (progDesc "Send signed transactions (reads from stdin)")
      , command "txn-status" $ info
          (cmdNodeTxnStatus <$> argTxId)
          (progDesc "Get the status of a transaction in the pool")
      ]

    argTxId :: Parser Text
    argTxId = strArgument $ mconcat
        [ metavar "<transaction id>"
        , help "ID of a transaction"
        ]

cmdNode :: MonadSubcommand m => Maybe NodeUrl -> (NodeUrl -> m ()) -> m ()
cmdNode murl sub = getNodeUrl murl >>= sub
  where
    getNodeUrl :: MonadSubcommand m => Maybe NodeUrl -> m NodeUrl
    getNodeUrl (Just u) = pure u
    getNodeUrl Nothing = asks goNetwork >>= \case
      "mainnet-v1.0" -> pure "https://api.algoexplorer.io/"
      "testnet-v1.0" -> pure "https://api.testnet.algoexplorer.io/"
      "betanet-v1.0" -> pure "https://api.betanet.algoexplorer.io/"
      net -> die $ "Unknown network `"+|net|+"`. Please, provide --url."

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
  connect url net >>= handleApiError . act

cmdNodeVersion :: MonadSubcommand m => NodeUrl -> m ()
cmdNodeVersion url = withNode url $ \(v, _) -> putJson v


-- | Fetch information about an account.
cmdNodeFetchAccount :: MonadSubcommand m => A.Address -> NodeUrl -> m ()
cmdNodeFetchAccount addr url = withNode url $ \(_, api) ->
  Api._account api addr >>= putJson

-- | Fetch a transaction (from the pool).
cmdNodeFetchTxn :: MonadSubcommand m => Text -> NodeUrl -> m ()
cmdNodeFetchTxn txId url = withNode url $ \(_, api) ->
  Api._transactionsPending api txId >>= putJson . Api.tiTxn

-- | Send transactions (or, actually, any bytes).
cmdNodeSend :: MonadSubcommand m => Bool -> NodeUrl -> m ()
cmdNodeSend json url = do
    bss <- case json of
      True -> map packTx <$> readItemsJson @T.SignedTransaction
      False -> readBytesB64
    withNode url $ \(_, api) ->
      mapM_ (Api._transactions api >=> putTextLn . Api.trTxId) bss
  where
    packTx = BSL.toStrict . MP.pack . MP.Canonical

-- | Get txn status
cmdNodeTxnStatus :: MonadSubcommand m => Text -> NodeUrl -> m ()
cmdNodeTxnStatus txId url = withNode url $ \(_, api) ->
  N.transactionStatus <$> Api._transactionsPending api txId >>= \case
    N.Waiting -> putTextLn $ "Waiting"
    N.KickedOut reason -> die $ "Kicked out: "+|reason|+""
    N.Confirmed r -> putTextLn . pretty $ "Confirmed in round " <> show r
