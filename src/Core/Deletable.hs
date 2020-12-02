module Core.Deletable
  ( Deletable(..)
  , deletable
  , deletableFromMaybe
  , maybeFromDeletable
  ) where

-- | A type of entity that can be either existing (created in the
-- past) or deleted from the system. It may be used to refer from
-- another entity. It is isomorphic to 'Maybe', but nested Maybes are
-- confusing and not self-documented.
data Deletable a
  = Deleted
  | Existing a
  deriving (Eq, Show)

instance Functor Deletable where
  fmap _ Deleted = Deleted
  fmap f (Existing a) = Existing (f a)

-- | Case analysis for 'Deletable'.
deletable :: b -> (a -> b) -> Deletable a -> b
deletable b _ Deleted = b
deletable _ f (Existing a) = f a

deletableFromMaybe :: Maybe a -> Deletable a
deletableFromMaybe = maybe Deleted Existing

maybeFromDeletable :: Deletable a -> Maybe a
maybeFromDeletable = deletable Nothing Just
