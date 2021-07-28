-- SPDX-FileCopyrightText: 2021 Serokell <https://serokell.io>
--
-- SPDX-License-Identifier: MPL-2.0

-- | Flags for commands
module Halgo.CLA.Flag
  ( flagB64
  , flagJson
  , flagVerify
  , flagGroupCheck
  ) where

import Options.Applicative (Parser, flag, help, long, short)

flagB64 :: Parser Bool
flagB64 = flag False True $ mconcat
  [ long "base64"
  , short 'b'
  , help "Print transactions as base64 instead of JSON"
  ]

flagJson :: Parser Bool
flagJson = flag False True $ mconcat
  [ long "json"
  , short 'j'
  , help "Read transactions as JSON instead of default base64"
  ]

flagVerify :: Parser Bool
flagVerify = flag True False $ mconcat
  [ long "no-verify"
  , short 'n'
  , help "Do not verify the signature of the transaction"
  ]

flagGroupCheck :: Parser Bool
flagGroupCheck = flag False True $ mconcat
  [ long "check"
  , help "Check that the transactions are a valid group instead of making a new one"
  ]
