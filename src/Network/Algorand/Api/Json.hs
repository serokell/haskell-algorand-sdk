-- SPDX-FileCopyrightText: 2021 Serokell <https://serokell.io>
--
-- SPDX-License-Identifier: MPL-2.0

{-# OPTIONS_GHC -Wno-orphans #-}

-- | JSON deriving options specific to Algorand and useful instancees.
--
-- Interestingly, different types in the algod API use differet style
-- for field names, so we have to provide multiple options.
module Network.Algorand.Api.Json
  ( algorandCamelOptions
  , algorandSnakeOptions
  , algorandTrainOptions
  , defaultOptions
  ) where

import qualified Data.Aeson (defaultOptions)
import qualified Data.Text as T

import Data.Aeson (FromJSON (..), Options (fieldLabelModifier, omitNothingFields), ToJSON (..))
import Data.Aeson.Casing (aesonPrefix, camelCase, snakeCase, trainCase)
import Data.Aeson.Types (parseFail)
import Data.ByteArray (ByteArray, Bytes, convert)
import Data.ByteArray.Sized (SizedByteArray, sizedByteArray, unSizedByteArray)
import Data.ByteString (ByteString)
import Data.ByteString.Base64 (decodeBase64, encodeBase64)
import Data.Text.Encoding (encodeUtf8)
import GHC.TypeLits (KnownNat)

defaultOptions :: Options
defaultOptions = Data.Aeson.defaultOptions { omitNothingFields = True }

-- | Options for types using camel case.
algorandCamelOptions :: Options
algorandCamelOptions = defaultOptions
  { fieldLabelModifier = fieldLabelModifier (aesonPrefix camelCase) }

-- | Options for types using snake case.
algorandSnakeOptions :: Options
algorandSnakeOptions = defaultOptions
  { fieldLabelModifier = fieldLabelModifier (aesonPrefix snakeCase) }

-- | Options for typees using train case.
algorandTrainOptions :: Options
algorandTrainOptions = defaultOptions
  { fieldLabelModifier = fieldLabelModifier (aesonPrefix trainCase) }

instance ToJSON ByteString where
  toJSON = toJSON . encodeBase64
  toEncoding = toEncoding . encodeBase64

instance FromJSON ByteString where
  parseJSON o = do
    b64 <- parseJSON o
    case decodeBase64 (encodeUtf8 b64) of
      Right bs -> pure bs
      Left err -> parseFail $ T.unpack err

instance ToJSON Bytes where
  toJSON = toJSON @ByteString . convert
  toEncoding = toEncoding @ByteString . convert

instance FromJSON Bytes where
  parseJSON = fmap convert . parseJSON @ByteString

instance (ByteArray b, ToJSON b, KnownNat n) => ToJSON  (SizedByteArray n b) where
  toJSON = toJSON . unSizedByteArray
  toEncoding = toEncoding . unSizedByteArray

instance (ByteArray b, FromJSON b, KnownNat n) => FromJSON (SizedByteArray n b) where
  parseJSON o = do
    bs <- parseJSON o
    case sizedByteArray bs of
      Nothing -> parseFail "Invalid bytes size"
      Just x -> pure x
