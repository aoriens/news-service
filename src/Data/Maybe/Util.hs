module Data.Maybe.Util
  ( fromMaybeM
  ) where

fromMaybeM :: Applicative m => m a -> Maybe a -> m a
fromMaybeM ifNothing = maybe ifNothing pure
