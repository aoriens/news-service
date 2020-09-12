module Web.Handler.GetAuthor
  ( run
  , Handle(..)
  ) where

import Control.Exception
import Core.Author
import qualified Core.Interactor.GetAuthor as I
import Data.Integral.Exact
import qualified Data.Text as T
import qualified Network.Wai as Wai
import Web.Credentials
import Web.Exception
import Web.Presenter.Author

data Handle =
  Handle
    { hGetAuthorHandle :: I.Handle IO
    , hPresenterHandle :: RepBuilderHandle
    }

run :: Handle -> Wai.Application
run h request respond = do
  credentials <- getCredentialsFromRequest request
  authorIdent <-
    maybe (throwIO NotFoundException) pure $
    parseAuthorId (Wai.pathInfo request)
  author <-
    maybe (throwIO NotFoundException) pure =<<
    I.run (hGetAuthorHandle h) credentials authorIdent
  respond $ presentAuthor (hPresenterHandle h) author

parseAuthorId :: [T.Text] -> Maybe AuthorId
parseAuthorId [s] = AuthorId <$> readExactIntegral (T.unpack s)
parseAuthorId _ = Nothing
