-- | Parsers of some data types from URI path components. It is always
-- assumed that leading path components before the data to be parsed
-- are removed.
module Web.PathParameter
  ( userIdFromPath
  , authorIdFromPathM
  , authorIdFromPath
  ) where

import Control.Monad.Catch
import Core.Author
import Core.User
import Data.Integral.Exact
import qualified Data.Text as T
import Web.Exception

type Path = [T.Text]

userIdFromPath :: Path -> Maybe UserId
userIdFromPath [s] = UserId <$> readExactIntegral (T.unpack s)
userIdFromPath _ = Nothing

authorIdFromPathM :: MonadThrow m => Path -> m AuthorId
authorIdFromPathM = maybe (throwM NotFoundException) pure . authorIdFromPath

authorIdFromPath :: Path -> Maybe AuthorId
authorIdFromPath [s] = AuthorId <$> readExactIntegral (T.unpack s)
authorIdFromPath _ = Nothing
