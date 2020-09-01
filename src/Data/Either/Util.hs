module Data.Either.Util
  ( eitherToMaybe
  ) where

eitherToMaybe :: Either e a -> Maybe a
eitherToMaybe = either (const Nothing) Just
