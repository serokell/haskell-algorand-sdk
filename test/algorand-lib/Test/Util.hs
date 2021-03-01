-- SPDX-FileCopyrightText: 2021 Serokell <https://serokell.io>
--
-- SPDX-License-Identifier: MPL-2.0

module Test.Util
  ( EitherError
  ) where


newtype EitherError a = EitherError (Either String a)
  deriving (Applicative, Eq, Functor, Monad)

instance Show a => Show (EitherError a) where
  show (EitherError e) = case e of
    Left err -> "error: " <> err
    Right a -> show a

instance MonadFail EitherError where
  fail = EitherError . Left
