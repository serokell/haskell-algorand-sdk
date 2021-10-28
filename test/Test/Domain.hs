-- SPDX-FileCopyrightText: 2021 Serokell <https://serokell.io>
--
-- SPDX-License-Identifier: MPL-2.0

{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Domain
  ( Signer (..)
  , signerSign

  , genesisHash
  , sender
  ) where

import qualified Data.Text as T

import Data.ByteString (ByteString)

import Crypto.Algorand.Key (SecretKey, skToText)
import Data.Algorand.Address (Address)
import Data.Algorand.Transaction (GenesisHash, Transaction)
import Data.Algorand.Transaction.Signed (SignedTransaction, signFromContractAccount, signSimple)

import Test.Util (genesisHashFromBytes)

-- | This is here only to make @tripping@ work.
instance Show SecretKey where
  show = T.unpack . skToText

-- | This is here only to make @tripping@ work.
instance Eq SecretKey where
  sk1 == sk2 = skToText sk1 == skToText sk2

data Signer
  = SignerSimple SecretKey
  | SignerContract ByteString
  deriving (Show)

signerSign :: Signer -> Transaction -> SignedTransaction
signerSign (SignerSimple sk) = signSimple sk
signerSign (SignerContract program) = signFromContractAccount program []

sender :: Address
sender = "BH55E5RMBD4GYWXGX5W5PJ5JAHPGM5OXKDQH5DC4O2MGI7NW4H6VOE4CP4"

genesisHash :: Maybe GenesisHash
genesisHash = genesisHashFromBytes "Mf0h6zjkEIEZPtNM3zsrg+iHQFS0fZxhgr7w35I464M="
