-- SPDX-FileCopyrightText: 2021 Serokell <https://serokell.io>
--
-- SPDX-License-Identifier: MPL-2.0

-- | The @halgo@ CLI.
module Main
  ( main
  ) where

import Control.Monad.Reader (runReaderT)
import Main.Utf8 (withUtf8)
import Options.Applicative (ParserInfo, ParserPrefs, customExecParser, disambiguate, helper,
                            hsubparser, info, prefs, progDesc, showHelpOnEmpty, showHelpOnError,
                            (<**>))

import Halgo.CLA (GlobalOptions, SubCommand, commands, globalOpts)

main :: IO ()
main = withUtf8 $ do
  (globalOptions, act) <- customExecParser preferences options
  runReaderT act globalOptions

preferences :: ParserPrefs
preferences = prefs $ mconcat
  [ disambiguate
  , showHelpOnEmpty
  , showHelpOnError
  ]

options :: ParserInfo (GlobalOptions, SubCommand)
options = info (cla <**> helper) $
  progDesc "A CLI tool for interacting with the Algorand blockchain"
  where
    cla = (,) <$> globalOpts <*> hsubparser commands
