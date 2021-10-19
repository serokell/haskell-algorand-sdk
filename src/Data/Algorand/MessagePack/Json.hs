-- SPDX-FileCopyrightText: 2021 Serokell <https://serokell.io>
--
-- SPDX-License-Identifier: MPL-2.0

-- | Canonical JSON via Canonoical MessagePack.
--
-- This is a pretty terrible hack that allows us to work with Algorand’s
-- “canonical” JSON values based on their canonical MessagePack encoding.
--
-- This module makes a bunch of weird assumptions, such as that all number
-- we have are @Word64@ and nothing else.
module Data.Algorand.MessagePack.Json
  ( toCanonicalJson
  , parseCanonicalJson
  ) where

import qualified Data.Aeson as JS
import qualified Data.Aeson.Types as JS
import qualified Data.HashMap.Strict as HM
import qualified Data.MessagePack as MP
import qualified Data.Scientific as S

import Data.ByteString (ByteString)
import Data.ByteString.Base64 (decodeBase64, encodeBase64)
import Data.Foldable (toList)
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)

import qualified Data.Algorand.Address as A

import Data.Algorand.MessagePack (Canonical (Canonical), EitherError (..), MessagePackObject,
                                  MessageUnpackObject, fromAlgoObject, toAlgoObject)

-- | Serialise to a canonical JSON.
toCanonicalJson :: (MessagePackObject a, MessageUnpackObject a) => a -> JS.Value
toCanonicalJson = jsonFromMsgpack . MP.toObject . Canonical

-- | Deserialise from a canonical JSON.
parseCanonicalJson :: (MessagePackObject a, MessageUnpackObject a) => JS.Value -> JS.Parser a
parseCanonicalJson v = do
  json <- JS.parseJSON v
  Canonical a <- MP.fromObject (msgpackFromJson json)
  pure a

-- | Turn an 'MP.Object' into a 'JS.Value'.
jsonFromMsgpack :: MP.Object -> JS.Value
jsonFromMsgpack MP.ObjectNil = JS.Null
jsonFromMsgpack (MP.ObjectBool b) = JS.toJSON b
jsonFromMsgpack (MP.ObjectInt i) = JS.toJSON i
jsonFromMsgpack (MP.ObjectWord w) = JS.toJSON w
jsonFromMsgpack (MP.ObjectFloat f) = JS.toJSON f
jsonFromMsgpack (MP.ObjectDouble d) = JS.toJSON d
jsonFromMsgpack (MP.ObjectStr s) = JS.toJSON s
jsonFromMsgpack (MP.ObjectBin bs) = JS.toJSON . encodeBase64 $ bs
jsonFromMsgpack (MP.ObjectArray arr) = JS.toJSON $ map jsonFromMsgpack arr
jsonFromMsgpack (MP.ObjectMap items) = JS.Object $ HM.fromList $ map pair items
  where
    pair :: (MP.Object, MP.Object) -> (Text, JS.Value)
    pair (k, v) = case MP.fromObject k of
      EitherError (Left err) -> error $ "Can only work with maps from text keys: " <> err
      EitherError (Right t) -> case (t, v) of
        ("snd", MP.ObjectBin (decodeAddressMsgpack -> Just js)) -> ("snd", js)
        ("rcv", MP.ObjectBin (decodeAddressMsgpack -> Just js)) -> ("rcv", js)
        _ -> (t, jsonFromMsgpack v)
jsonFromMsgpack (MP.ObjectExt _t _d) = error "ObjectExt is not supported"

-- | Turn a 'JS.Value' into an 'MP.Object'.
msgpackFromJson :: JS.Value -> MP.Object
msgpackFromJson (JS.Object o) =
    MP.ObjectMap . map mapItem . HM.toList $ o
  where
    mapItem :: (Text, JS.Value) -> (MP.Object, MP.Object)
    -- Special hack for the only string field that we have in the protocol.
    mapItem ("type", JS.String s) = (MP.ObjectStr "type", MP.ObjectStr s)
    -- Special hack for addresses, which are encoded as base32
    mapItem ("snd", JS.String (decodeAddressJson -> Just mp)) = (MP.ObjectStr "snd", mp)
    mapItem ("rcv", JS.String (decodeAddressJson -> Just mp)) = (MP.ObjectStr "rcv", mp)
    -- Default case
    mapItem (k, v) = (MP.toObject k, msgpackFromJson v)
msgpackFromJson (JS.Array arr) =
  MP.toObject $ map msgpackFromJson $ toList arr
msgpackFromJson (JS.String s) =
  case decodeBase64 (encodeUtf8 s) of
    Right bs -> MP.ObjectBin bs
    Left _ -> MP.ObjectStr s
msgpackFromJson (JS.Number n) =
  case S.toBoundedInteger n of
    Just w -> MP.ObjectWord w
    Nothing -> error "Only Word64 is supported as Number"
msgpackFromJson (JS.Bool b) = MP.ObjectBool b
msgpackFromJson JS.Null = MP.ObjectNil

decodeAddressJson :: Text -> Maybe MP.Object
decodeAddressJson = fmap toAlgoObject . A.fromText

decodeAddressMsgpack :: ByteString -> Maybe JS.Value
decodeAddressMsgpack = fmap JS.toJSON . fromAlgoObject @A.Address . MP.ObjectBin
