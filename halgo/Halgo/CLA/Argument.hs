-- SPDX-FileCopyrightText: 2021 Serokell <https://serokell.io>
--
-- SPDX-License-Identifier: MPL-2.0

-- | Common arguments for commands.
module Halgo.CLA.Argument
  ( argSecretFile
  , argProgramSourceFile
  , argProgramFile
  , argAddress
  , argTxId
  , argAmount
  , argAssetIndex
  ) where

import qualified Data.Text as T

import Data.Text (Text)
import Options.Applicative (Parser, action, argument, auto, eitherReader, help, metavar,
                            strArgument)

import qualified Data.Algorand.Address as A

import Data.Algorand.Address (Address)
import Data.Algorand.Amount (Microalgos)
import Data.Algorand.Asset (AssetIndex)

argSecretFile :: Parser FilePath
argSecretFile = strArgument $ mconcat
  [ metavar "<key file>"
  , help "Path to a file of the account"
  , action "file"
  ]

argProgramSourceFile :: Parser FilePath
argProgramSourceFile = strArgument $ mconcat
  [ metavar "<program source file>"
  , help "Path to a file with a contract source code"
  , action "file"
  ]

argProgramFile :: Parser FilePath
argProgramFile = strArgument $ mconcat
  [ metavar "<program file>"
  , help "Path to a file with a compiled contract code"
  , action "file"
  ]

argAddress :: String -> Parser Address
argAddress helpText = argument reader $ mconcat
  [ metavar "<address>"
  , help helpText
  ]
  where
    reader = eitherReader $ \s -> case A.fromText (T.pack s) of
      Nothing -> Left "Malformed address."
      Just a -> Right a

argTxId :: Parser Text
argTxId = strArgument $ mconcat
  [ metavar "<transaction id>"
  , help "ID of a transaction"
  ]

argAmount :: Parser Microalgos
argAmount = argument auto $ mconcat
  [ metavar "<amount>"
  , help "Amount in microalgos"
  ]

argAssetIndex :: Parser AssetIndex
argAssetIndex = argument auto (metavar "<asset>" <> help "Index of the asset")
