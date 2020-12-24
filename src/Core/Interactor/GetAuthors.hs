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
    , hPageSpecParserHandle :: PageSpecParserHandle
    }

run ::
     MonadThrow m
  => Handle m
  -> AuthenticatedUser
  -> PageSpecQuery
  -> m [Author]
run Handle {..} authUser pageQuery = do
  authorize "get authors" $ authUserShouldBeAdmin authUser
  page <- parsePageSpecM hPageSpecParserHandle pageQuery
  hGetAuthors page
