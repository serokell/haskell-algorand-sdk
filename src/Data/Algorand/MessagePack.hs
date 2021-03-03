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

  , AlgorandMessagePack (..)
  , (&<>)
  , (.=<)

  , AlgorandMessageUnpack (..)
  , (.:)
  , (.:?)
  , (.:>)

  , Canonical (..)

  , EitherError (..)

  , module Data.MessagePack
  ) where


import Control.Monad ((>=>))
import Data.ByteArray (ByteArray, Bytes, convert)
import Data.ByteArray.Sized (SizedByteArray, sizedByteArray, unSizedByteArray)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Default.Class (Default (def))
import Data.Function ((&))
import Data.List (sortOn)
import Data.MessagePack (Assoc (Assoc, unAssoc), MessagePack (fromObject, toObject), Object, toObject, pack, unpack)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Word (Word64)
import GHC.TypeLits (KnownNat)


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

instance MessagePack CanonicalObject where
  toObject = toObject . Assoc . sortOn fst . unCanonicalObject
  fromObject = fmap (CanonicalObject . unAssoc) . fromObject

-- | Add a key to a 'CanonicalObject'.
(.=) :: (NonZeroValue a, MessagePack a) => Text -> a -> CanonicalObject -> CanonicalObject
infixl 2 .=
k .= v
  | isNonZero v = (CanonicalObject [(k, toObject v)] <>)
  | otherwise = id

-- | Like 'MessagePack' but specifically for the Algorand’s canonical flavour.
-- This class is only for encoding, 'AlgorandMessageUnpack' is for decoding.
class AlgorandMessagePack a where
  -- Like 'toObject', but produces a canonical representation.
  toCanonicalObject :: a -> CanonicalObject

instance AlgorandMessagePack a => AlgorandMessagePack (Maybe a) where
  toCanonicalObject Nothing = mempty
  toCanonicalObject (Just a) = toCanonicalObject a

-- | Append an object to another one.
(&<>) :: AlgorandMessagePack a => CanonicalObject -> a -> CanonicalObject
infixl 0 &<>
o &<> a = o <> toCanonicalObject a

-- | Store a canonical suboject.
(.=<) :: AlgorandMessagePack a => Text -> a -> CanonicalObject -> CanonicalObject
infixl 2 .=<
k .=< v = k .= toCanonicalObject v


-- | The decoding counterpart of 'AlgorandMessagePack'.
class AlgorandMessageUnpack a where
  -- Like 'fromObject', but fills in defaulted values.
  fromCanonicalObject :: MonadFail m => CanonicalObject -> m a

instance AlgorandMessageUnpack a => AlgorandMessageUnpack (Maybe a) where
  fromCanonicalObject = fmap Just . fromCanonicalObject

-- | Lookup a key in a canonical object for a type that cannot be defaulted.
(.:) :: (MonadFail m, MessagePack a) => CanonicalObject -> Text -> m a
infixl 2 .:
CanonicalObject o .: k = case lookup k o of
  Nothing -> fail $ "Missing required key: " <> T.unpack k
  Just v -> fromObject v

-- | Lookup ankey in a canonical object.
(.:?) :: (MonadFail m, MessagePack a, CanonicalZero a) => CanonicalObject -> Text -> m a
infixl 2 .:?
CanonicalObject o .:? k = case lookup k o of
  Nothing -> pure zero
  Just v -> fromObject v

-- | Lookup a canonical suboject.
(.:>) :: (MonadFail m, AlgorandMessageUnpack a) => CanonicalObject -> Text -> m a
infixl 2 .:>
o .:> k = o .:? k >>= fromCanonicalObject


-- | A wrapper with its 'MessagePack' instance goin via 'CanonicalObject'.
newtype Canonical a = Canonical { unCanonical :: a }

instance (AlgorandMessagePack a, AlgorandMessageUnpack a) => MessagePack (Canonical a) where
  toObject = toObject . toCanonicalObject . unCanonical
  fromObject = fromObject >=> fromCanonicalObject >=> pure . Canonical


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
 -}

instance MessagePack Bytes where
  toObject = toObject @ByteString . convert
  fromObject = fmap convert . fromObject @ByteString

instance (ByteArray b, KnownNat n, MessagePack b) => MessagePack (SizedByteArray n b) where
  toObject = toObject . unSizedByteArray
  fromObject o = do
    bs <- fromObject o
    case sizedByteArray bs of
      Nothing -> fail "Invalid bytes size"
      Just x -> pure x
