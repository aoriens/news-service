module Web.Handler.DeleteUser
  ( run
  , Handle(..)
  ) where

import qualified Network.Wai as Wai
import qualified Core.Interactor.DeleteUser as I
import Core.DTO.User
import qualified Data.Text as T
import Data.Int.Exact

newtype Handle =
  Handle
    { hDeleteUserHandle :: I.Handle IO
    }

run :: Handle -> Wai.Application
run h request respond = do
  credentials <- credentialsFromRequest request
  userIdent <- parseUserId $ Wai.pathInfo request
  I.run (hDeleteUserHandle h) credentials userIdent

parseUserId :: [T.Text] -> Maybe UserId
parseUserId [s] = UserId <$> readExactIntegral (T.unpack s)
parseUserId _ = Nothing
