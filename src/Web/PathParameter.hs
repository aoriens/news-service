-- | Parsers of some data types from URI path components. It is always
-- assumed that leading path components before the data to be parsed
-- are removed.
module Web.PathParameter
  ( getUserIdFromPath
  , getAuthorIdFromPath
  ) where

import Control.Monad.Catch
import Core.Author
import Core.User
import Data.Integral.Exact
import qualified Data.Text as T
import Web.Exception

type Path = [T.Text]

getUserIdFromPath :: MonadThrow m => Path -> m UserId
getUserIdFromPath path =
  notFoundIfNothing $ do
    [s] <- pure path
    UserId <$> readExactIntegral (T.unpack s)

getAuthorIdFromPath :: MonadThrow m => Path -> m AuthorId
getAuthorIdFromPath path =
  notFoundIfNothing $ do
    [s] <- pure path
    AuthorId <$> readExactIntegral (T.unpack s)

notFoundIfNothing :: MonadThrow m => Maybe a -> m a
notFoundIfNothing Nothing = throwM NotFoundException
notFoundIfNothing (Just x) = pure x
