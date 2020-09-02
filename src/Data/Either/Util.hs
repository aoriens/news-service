module Data.Either.Util
  ( eitherToMaybe
  , maybeToEither
  ) where

eitherToMaybe :: Either e a -> Maybe a
eitherToMaybe = either (const Nothing) Just

maybeToEither :: e -> Maybe a -> Either e a
maybeToEither e = maybe (Left e) Right
