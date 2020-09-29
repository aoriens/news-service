module Core.Interactor.GetAuthors
  ( run
  , Handle(..)
  ) where

import Control.Monad.Catch
import Core.Author
import Core.Authorization
import Core.Pagination

data Handle m =
  Handle
    { hGetAuthors :: PageSpec -> m [Author]
    , hAuthenticationHandle :: AuthenticationHandle m
    , hAuthorizationHandle :: AuthorizationHandle
    , hPageSpecParserHandle :: PageSpecParserHandle
    }

run ::
     MonadThrow m
  => Handle m
  -> Maybe Credentials
  -> PageSpecQuery
  -> m [Author]
run Handle {..} credentials pageQuery = do
  actor <- authenticate hAuthenticationHandle credentials
  requireAdminPermission hAuthorizationHandle actor "get authors"
  page <- parsePageSpecM hPageSpecParserHandle pageQuery
  hGetAuthors page
