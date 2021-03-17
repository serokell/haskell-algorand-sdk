-- SPDX-FileCopyrightText: 2021 Serokell <https://serokell.io>
--
-- SPDX-License-Identifier: MPL-2.0

{-# OPTIONS_GHC -Wno-orphans #-}

-- | Canonical Algorand MessagePack encoding.
--
-- Quoting @py-algorand-sdk@:
--
--      Canonical Msgpack: maps must contain keys in lexicographic order; maps
--      must omit key-value pairs where the value is a zero-value; positive
--      integer values must be encoded as "unsigned" in msgpack, regardless of
--      whether the value space is semantically signed or unsigned; integer
--      values must be represented in the shortest possible encoding; binary
--      arrays must be represented using the "bin" format family (that is, use
--      the most recent version of msgpack rather than the older msgpack
--      version that had no "bin" family).
module Data.Algorand.MessagePack
  ( CanonicalZero (..)
  , NonZeroValue (..)

  , CanonicalObject
  , (&)
  , (.=)

  , MessagePackObject (..)
  , (&<>)
  , (.=<)

  , MessageUnpackObject (..)
  , (.:)
  , (.:?)
  , (.:??)
  , (.:>)
  , (.:>?)

  , Canonical (..)

  , EitherError (..)

  , module Data.MessagePack
  , AlgoMessagePack (..)
  ) where


import Control.Monad ((>=>))
import Data.ByteArray (ByteArray, Bytes, convert)
import Data.ByteArray.Sized (SizedByteArray, sizedByteArray, unSizedByteArray)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Default.Class (Default (def))
import Data.Function ((&))
import Data.List (sortOn)
import Data.MessagePack (Assoc (Assoc, unAssoc), MessagePack (fromObject, toObject), Object (ObjectArray, ObjectNil), toObject, pack, unpack)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Word (Word64)
import GHC.TypeLits (KnownNat)


-- | Our own 'MessagePack' class, becuase the standard one has some weird instances.
--
-- FIXME: This class should not exist, it should just be 'MessagePack'.
-- https://github.com/TokTok/hs-msgpack-types/pull/37
class AlgoMessagePack a where
  toAlgoObject :: a -> Object
  fromAlgoObject :: MonadFail m => Object -> m a

instance AlgoMessagePack ByteString where
  toAlgoObject = toObject
  fromAlgoObject = fromObject

instance AlgoMessagePack Word64 where
  toAlgoObject = toObject
  fromAlgoObject = fromObject

instance AlgoMessagePack Text where
  toAlgoObject = toObject
  fromAlgoObject = fromObject

instance AlgoMessagePack a => AlgoMessagePack [a] where
  toAlgoObject = ObjectArray . map toAlgoObject
  fromAlgoObject = \case
    ObjectArray xs -> mapM fromAlgoObject xs
    _              -> fail "invalid encoding for list"

instance (AlgoMessagePack a) => AlgoMessagePack (Maybe a) where
  toAlgoObject Nothing = ObjectNil
  toAlgoObject (Just a) = toAlgoObject a
  fromAlgoObject = fmap Just . fromAlgoObject


-- | Class for data types that have a reasonable “canonical zero” value.
class CanonicalZero a where
  zero :: a
  default zero :: Default a => a
  zero = def

-- | Class for data types that have one or many “canonoical zero” values which
-- should not be encoded.
class NonZeroValue a where
  isNonZero :: a -> Bool
  default isNonZero :: (Eq a, CanonicalZero a) => a -> Bool
  isNonZero = (/= zero)

instance CanonicalZero Word64
instance NonZeroValue Word64

instance NonZeroValue ByteString where
  isNonZero = not . BS.null

instance NonZeroValue Text where
  isNonZero = not . T.null

instance CanonicalZero (Maybe a)
instance NonZeroValue a => NonZeroValue (Maybe a) where
  isNonZero (Just a) = isNonZero a
  isNonZero Nothing = False

instance CanonicalZero [a]
instance NonZeroValue [a] where
  isNonZero = not . null

instance NonZeroValue (SizedByteArray 32 b) where
  isNonZero _ = True


-- | An intermediate representation for non-primitive MessagePack objects
-- that makes it easier to ensure that it is canonical.
newtype CanonicalObject = CanonicalObject { unCanonicalObject :: [(Text, Object)] }
  deriving (CanonicalZero, Monoid, NonZeroValue, Semigroup)

instance AlgoMessagePack CanonicalObject where
  toAlgoObject = toObject . Assoc . sortOn fst . unCanonicalObject
  fromAlgoObject = fmap (CanonicalObject . unAssoc) . fromObject

-- | Add a key to a 'CanonicalObject'.
(.=) :: (NonZeroValue a, AlgoMessagePack a) => Text -> a -> CanonicalObject -> CanonicalObject
infixl 2 .=
k .= v
  | isNonZero v = (CanonicalObject [(k, toAlgoObject v)] <>)
  | otherwise = id

-- | Like 'MessagePack' but specifically for the Algorand’s canonical flavour.
-- This class is only for encoding, 'MessageUnpackObject' is for decoding.
class MessagePackObject a where
  -- Like 'toObject', but produces a canonical representation.
  toCanonicalObject :: a -> CanonicalObject

instance MessagePackObject a => MessagePackObject (Maybe a) where
  toCanonicalObject Nothing = mempty
  toCanonicalObject (Just a) = toCanonicalObject a

-- | Append an object to another one.
(&<>) :: MessagePackObject a => CanonicalObject -> a -> CanonicalObject
infixl 0 &<>
o &<> a = o <> toCanonicalObject a

-- | Store a canonical suboject.
(.=<) :: MessagePackObject a => Text -> a -> CanonicalObject -> CanonicalObject
infixl 2 .=<
k .=< v = k .= toCanonicalObject v


-- | The decoding counterpart of 'MessagePackObject'.
class MessageUnpackObject a where
  -- Like 'fromObject', but fills in defaulted values.
  fromCanonicalObject :: MonadFail m => CanonicalObject -> m a

instance MessageUnpackObject a => MessageUnpackObject (Maybe a) where
  fromCanonicalObject = fmap Just . fromCanonicalObject

-- | Lookup a key in a canonical object for a type that cannot be defaulted.
(.:) :: (MonadFail m, AlgoMessagePack a) => CanonicalObject -> Text -> m a
infixl 2 .:
CanonicalObject o .: k = case lookup k o of
  Nothing -> fail $ "Missing required key: " <> T.unpack k
  Just v -> fromAlgoObject v

-- | Lookup a key in a canonical object.
(.:?) :: (MonadFail m, AlgoMessagePack a, CanonicalZero a) => CanonicalObject -> Text -> m a
infixl 2 .:?
CanonicalObject o .:? k = case lookup k o of
  Nothing -> pure zero
  Just v -> fromAlgoObject v

-- | Lookup a key in a canonical, if it exists.
(.:??) :: (MonadFail m, AlgoMessagePack a) => CanonicalObject -> Text -> m (Maybe a)
infixl 2 .:??
CanonicalObject o .:?? k = case lookup k o of
  Nothing -> pure Nothing
  Just v -> Just <$> fromAlgoObject v

-- | Lookup a canonical suboject.
(.:>) :: (MonadFail m, MessageUnpackObject a) => CanonicalObject -> Text -> m a
infixl 2 .:>
o .:> k = o .:? k >>= fromCanonicalObject

-- | Lookup a canonical suboject, if the key exists.
(.:>?) :: (MonadFail m, MessageUnpackObject a) => CanonicalObject -> Text -> m (Maybe a)
infixl 2 .:>?
o .:>? k = o .:?? k >>= \case
  Nothing -> pure Nothing
  Just v -> Just <$> fromCanonicalObject v


-- | A wrapper with its 'MessagePack' instance goin via 'CanonicalObject'.
newtype Canonical a = Canonical { unCanonical :: a }

instance (MessagePackObject a, MessageUnpackObject a) => MessagePack (Canonical a) where
  toObject = toAlgoObject . toCanonicalObject . unCanonical
  fromObject = fromAlgoObject >=> fromCanonicalObject >=> pure . Canonical


-- | A helper for using with @unpack@.
newtype EitherError a = EitherError (Either String a)
  deriving (Applicative, Eq, Functor, Monad)

instance Show a => Show (EitherError a) where
  show (EitherError e) = case e of
    Left err -> "error: " <> err
    Right a -> show a

instance MonadFail EitherError where
  fail = EitherError . Left


{-
 - A bunch of orphan instances that we need.
 - (These used to be MessagePack instances, before the introduction of AlgoMessagePack)
 -}

instance AlgoMessagePack Bytes where
  toAlgoObject = toAlgoObject @ByteString . convert
  fromAlgoObject = fmap convert . fromAlgoObject @ByteString

instance (ByteArray b, KnownNat n, AlgoMessagePack b) => AlgoMessagePack (SizedByteArray n b) where
  toAlgoObject = toAlgoObject . unSizedByteArray
  fromAlgoObject o = do
    bs <- fromAlgoObject o
    case sizedByteArray bs of
      Nothing -> fail "Invalid bytes size"
      Just x -> pure x
