module Web.Handler.DeleteAuthor
  ( run
  , Handle(..)
  ) where

import Core.Author
import qualified Core.Interactor.DeleteAuthor as I
import Data.Integral.Exact
import qualified Data.Text as T
import qualified Network.HTTP.Types as Http
import qualified Network.Wai as Wai
import Web.Credentials

newtype Handle =
  Handle
    { hDeleteAuthorHandle :: I.Handle IO
    }

run :: Handle -> Wai.Application
run Handle {..} request respond = do
  case parseAuthorId $ Wai.pathInfo request of
    Just authorIdent -> do
      credentials <- getCredentialsFromRequest request
      I.run hDeleteAuthorHandle credentials authorIdent
    Nothing -> pure ()
  respond $ Wai.responseLBS Http.noContent204 [] mempty

parseAuthorId :: [T.Text] -> Maybe AuthorId
parseAuthorId [s] = AuthorId <$> readExactIntegral (T.unpack s)
parseAuthorId _ = Nothing
