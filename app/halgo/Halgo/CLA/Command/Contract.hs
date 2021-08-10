-- SPDX-FileCopyrightText: 2021 Serokell <https://serokell.io>
--
-- SPDX-License-Identifier: MPL-2.0

-- | Work with TEAL programs commands.
module Halgo.CLA.Command.Contract
  ( contractOpts
  ) where

import qualified Data.ByteString as BS
import qualified Data.Text.IO as TIO

import Fmt ((+|), (|+))
import Options.Applicative (Parser, command, hsubparser, info, progDesc)
import UnliftIO (liftIO)

import qualified Data.Algorand.Address as A
import qualified Network.Algorand.Api as Api

import Network.Algorand.Definitions (Host)
import Data.Algorand.Teal (TealCode (..), TealCompilationResult (..))


import Halgo.CLA.Argument (argProgramFile, argProgramSourceFile)
import Halgo.CLA.Command.Node (cmdNode, optNodeHost)
import Halgo.CLA.Type (MonadSubCommand, SubCommand)
import Halgo.IO (putNoticeLn, putTextLn)
import Halgo.Util (withNode)

contractOpts :: Parser SubCommand
contractOpts = hsubparser $ mconcat
  [ command "compile"
    $ info (cmdNode <$> optNodeHost <*> (cmdContractCompile <$> argProgramSourceFile))
    $ progDesc "Compile source code to binary. Appends `.tok` to the input file name."

  , command "address"
    $ info (cmdContractAddress <$> argProgramFile)
    $ progDesc "Show the address of the compiled contract account"
  ]

cmdContractCompile :: MonadSubCommand m => FilePath -> Host -> m ()
cmdContractCompile sourcePath url = withNode url $ \(_, api) -> do
  source <- liftIO $ TIO.readFile sourcePath
  bin <- unTealCode . tcrResult <$> Api._compileTeal api source
  let outPath = sourcePath <> ".tok"
  putNoticeLn $ "Writing compiled program to `"+|outPath|+"`"
  liftIO $ BS.writeFile outPath bin
  putTextLn (A.toText . A.fromContractCode $ bin)

cmdContractAddress :: MonadSubCommand m => FilePath -> m ()
cmdContractAddress programPath = do
  program <- liftIO $ BS.readFile programPath
  putTextLn (A.toText . A.fromContractCode $ program)
