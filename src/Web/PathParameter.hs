-- | Parsers of some data types from URI path components. It is always
-- assumed that leading path components before the data to be parsed
-- are removed.
module Web.PathParameter
  ( userIdFromPath
  , authorIdFromPath
  ) where

import Core.Author
import Core.User
import Data.Integral.Exact
import qualified Data.Text as T

type Path = [T.Text]

userIdFromPath :: Path -> Maybe UserId
userIdFromPath [s] = UserId <$> readExactIntegral (T.unpack s)
userIdFromPath _ = Nothing

authorIdFromPath :: Path -> Maybe AuthorId
authorIdFromPath [s] = AuthorId <$> readExactIntegral (T.unpack s)
authorIdFromPath _ = Nothing
