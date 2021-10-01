-- SPDX-FileCopyrightText: 2021 Serokell <https://serokell.io>
--
-- SPDX-License-Identifier: MPL-2.0

-- | Helpers for reading and printing things.
module Halgo.IO
  ( putTextLn
  , putJson
  , putNoticeLn

  , readItemsJson
  , readItemsB64

  , putItemsJson
  , putItemsB64
  ) where

import Control.Monad (forM)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Aeson (FromJSON, ToJSON)
import qualified Data.Aeson as JS
import qualified Data.Aeson.Types as JS
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.ByteString.Base64 (decodeBase64, encodeBase64)
import qualified Data.ByteString.Lazy as BSL
import Data.Text (Text)
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as T
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Encoding as LT
import qualified Data.Text.Lazy.IO as LT
import Fmt (pretty)
import System.IO (stderr)

import qualified Data.Algorand.MessagePack as MP

import Halgo.Util (die)



-- | @putStrLn@ for 'Text'.
putTextLn :: MonadIO m => Text -> m ()
putTextLn = liftIO . T.putStrLn

-- | Encode as JSON and pretty-print to stdout.
putJson :: (MonadIO m, ToJSON a) => a -> m ()
putJson = liftIO . LT.putStrLn . LT.decodeUtf8 . encodePretty

-- | @putStrLn@ for 'Text' that prints to 'stderr'.
putNoticeLn :: MonadIO m => Text -> m ()
putNoticeLn = liftIO . T.hPutStrLn stderr


-- | Read a list of base64-encoded bytestrings, one per line.
readBytesB64 :: MonadIO m => m [BS.ByteString]
readBytesB64 = do
  ls <- LT.words <$> liftIO LT.getContents
  forM ls $ \line -> do
    case decodeBase64 . T.encodeUtf8 . LT.toStrict $ line of
      Left err -> die $ pretty err
      Right bs -> pure bs

-- | Read a list of items encoded as JSON.
--
-- The input can be either:
--   * a single JSON-encoded item (will return a list with one item)
--   * or a JSON-encoded array of items.
readItemsJson :: (FromJSON d, MonadIO m) => m [d]
readItemsJson = liftIO BSL.getContents >>= itemsFromJson
  where
    -- This function is being smart and detects whether its input
    -- is an encoding of an array or not.
    itemsFromJson bs = case JS.eitherDecode bs of
      Right o@(JS.Array _) -> case JS.parseEither JS.parseJSON o of
        Right items -> pure items
        Left err -> die $ pretty err
      Right o -> case JS.parseEither JS.parseJSON o of
        Right item -> pure [item]
        Left err -> die $ pretty err
      Left err -> die $ pretty err

-- | Read a list of base64-encoded items, one per line.
readItemsB64 :: forall d m. (MP.MessagePack (MP.Canonical d), MonadIO m) => m [d]
readItemsB64 = readBytesB64 >>= mapM itemFromBytes
  where
    itemFromBytes :: ByteString -> m d
    itemFromBytes bs = case MP.unpack (BSL.fromStrict bs) of
      MP.EitherError (Left err) -> die $ pretty err
      MP.EitherError (Right (MP.Canonical r)) -> pure r


-- | Print a list of items encoded as JSON.
--
-- If the list contains exactly one item, the resulting encoding will
-- be just one object rather than an array. In this sense, this function
-- is an inverse of 'readItemsJson'.
putItemsJson :: (ToJSON d, MonadIO m) => [d] -> m ()
putItemsJson [] = pure ()
putItemsJson [item] = putJson item
putItemsJson items = putJson items


-- | Print a list of items base64-encoded, one per line.
putItemsB64 :: (MP.MessagePack (MP.Canonical d), MonadIO m) => [d] -> m ()
putItemsB64 = mapM_ (liftIO . T.putStrLn . encodeBase64 . BSL.toStrict . MP.pack . MP.Canonical)
