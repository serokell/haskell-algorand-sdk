-- SPDX-FileCopyrightText: 2021 Serokell <https://serokell.io>
--
-- SPDX-License-Identifier: MPL-2.0

-- | Work with transactions commands.
module Halgo.CLA.Command.Transaction
  ( transactionOpts
  ) where

import qualified Data.ByteString as BS

import Control.Monad (forM_, when)
import Options.Applicative (Parser, command, hsubparser, info, progDesc)
import UnliftIO (liftIO)

import qualified Network.Algorand.Api as Api

import Data.Algorand.Address (Address, zero)
import Data.Algorand.Amount (Microalgos)
import Data.Algorand.Asset (AssetIndex)
import Data.Algorand.Transaction (Transaction, TransactionType (..), transactionId)
import Data.Algorand.Transaction.Build (buildTransaction)
import Data.Algorand.Transaction.Group (isValidGroup, makeGroup)
import Data.Algorand.Transaction.Signed (signFromContractAccount, signSimple, verifyTransaction)
import Network.Algorand.Definitions (Host)

import Halgo.CLA.Argument (argAddress, argAmount, argAssetIndex, argProgramFile, argSecretFile)
import Halgo.CLA.Command.Account (loadAccount)
import Halgo.CLA.Command.Node (cmdNode, optNodeHost)
import Halgo.CLA.Flag (flagB64, flagGroupCheck, flagJson, flagVerify)
import Halgo.CLA.Type (MonadSubCommand, SubCommand)
import Halgo.IO (putItemsB64, putItemsJson, putTextLn, readItemsB64, readItemsJson)
import Halgo.Util (die, withNode)

transactionOpts :: Parser SubCommand
transactionOpts = hsubparser $ mconcat
  [ command "show"
    $ info (cmdTxnShow <$> flagVerify <*> flagJson <*> flagB64)
    $ progDesc "Decode from base64 and display signed transactions (reads from stdin)"

  , command "show-unsigned"
    $ info (cmdTxnShowUnsigned <$> flagJson <*> flagB64)
    $ progDesc "Decode from base64 and display unsigned transactions (reads from stdin)"

  , command "sign"
    $ info (cmdTxnSign <$> flagJson <*> argSecretFile)
    $ progDesc "Sign one or multiple transactions with a simple sig (reads from stdin)"

  , command "lsign"
    $ info (cmdTxnLogicSign <$> flagJson <*> argProgramFile)
    $ progDesc "Sign one or multiple transactions with a logic sig (reads from stdin)"

  , command "id"
    $ info (cmdTxnId <$> flagJson)
    $ progDesc "Calculate transaction ID"

  , command "new"
    $ info (cmdNode <$> optNodeHost <*> hsubparser (mconcat
      [ command "pay"
        $ info (cmdTxnNewPay <$> argAddress "Receiver" <*> argAmount)
        $ progDesc "Create a new Payment transaction"

      , command "axfr"
        $ info (cmdTxnNewAxfr <$> argAssetIndex <*> argAddress "Receiver" <*> argAmount)
        $ progDesc "Create a new AssetTransfer transaction"
      ]))
    $ progDesc "Create a new transaction"

  , command "group"
    $ info (cmdTxnGroup <$> flagJson <*> flagGroupCheck)
    $ progDesc "Collect transactions into a group"
  ]

-- | Show signed transactions.
cmdTxnShow :: MonadSubCommand m => Bool -> Bool -> Bool -> m ()
cmdTxnShow verify json base64 = do
  stxns <- if json then readItemsJson else readItemsB64
  when verify $
    forM_ stxns $ \stxn -> case verifyTransaction stxn of
      Nothing -> die "Invalid signature. Run with --no-verify if you still want to see it."
      Just _txn -> pure ()
  (if base64 then putItemsB64 else putItemsJson) stxns

-- | Show unsigned transactions.
cmdTxnShowUnsigned :: MonadSubCommand m => Bool -> Bool -> m ()
cmdTxnShowUnsigned json base64 =
  (if json then readItemsJson else readItemsB64 @Transaction)
  >>= if base64 then putItemsB64 else putItemsJson

-- | Sign transactions with a simple signature.
cmdTxnSign :: MonadSubCommand m => Bool -> FilePath -> m ()
cmdTxnSign json skFile = do
  sk <- loadAccount skFile
  txns <- if json then readItemsJson else readItemsB64
  let signed = map (signSimple sk) txns
  putItemsB64 signed

-- | Sign transactions with a logic signature.
cmdTxnLogicSign :: MonadSubCommand m => Bool -> FilePath -> m ()
cmdTxnLogicSign json programFile = do
  program <- liftIO $ BS.readFile programFile
  txns <- if json then readItemsJson else readItemsB64
  let signed = map (signFromContractAccount program []) txns
  putItemsB64 signed

-- | Transaction ID.
cmdTxnId :: MonadSubCommand m => Bool -> m ()
cmdTxnId json = do
  txns <- if json then readItemsJson else readItemsB64
  mapM_ (putTextLn . transactionId) txns

-- | Create or check a group of transactions
cmdTxnGroup :: MonadSubCommand m => Bool -> Bool -> m ()
cmdTxnGroup json check = do
  txns <- if json then readItemsJson else readItemsB64
  case check of
    True -> case isValidGroup txns of
      False -> die "Not a valid transaction group"
      True -> putItemsB64 txns
    False -> putItemsB64 $ makeGroup txns

cmdTxnNewPay :: MonadSubCommand m => Address -> Microalgos -> Host -> m ()
cmdTxnNewPay to amnt url = withNode url $ \(_, api) -> do
  params <- Api._transactionsParams api
  let payment = PaymentTransaction to amnt Nothing
  let txn = buildTransaction params zero payment
  putItemsB64 [txn]

cmdTxnNewAxfr
  :: MonadSubCommand m
  => AssetIndex -> Address -> Microalgos -> Host -> m ()
cmdTxnNewAxfr index to amnt url = withNode url $ \(_, api) -> do
  params <- Api._transactionsParams api
  let payment = AssetTransferTransaction index amnt Nothing to Nothing
  let txn = buildTransaction params zero payment
  putItemsB64 [txn]
