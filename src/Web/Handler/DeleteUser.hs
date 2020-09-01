module Web.Handler.DeleteUser
  ( run
  , Handle(..)
  ) where

import Core.DTO.User
import qualified Core.Interactor.DeleteUser as I
import Data.Int.Exact
import qualified Data.Text as T
import qualified Network.HTTP.Types as Http
import qualified Network.Wai as Wai
import Web.Credentials

newtype Handle =
  Handle
    { hDeleteUserHandle :: I.Handle IO
    }

run :: Handle -> Wai.Application
run h request respond = do
  case parseUserId $ Wai.pathInfo request of
    Just uid -> do
      credentials <- getCredentialsFromRequest request
      I.run (hDeleteUserHandle h) credentials uid
    Nothing -> pure ()
  respond $ Wai.responseLBS Http.noContent204 [] mempty

parseUserId :: [T.Text] -> Maybe UserId
parseUserId [s] = UserId <$> readExactIntegral (T.unpack s)
parseUserId _ = Nothing
