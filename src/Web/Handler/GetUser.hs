module Web.Handler.GetUser
  ( run
  , Handle(..)
  ) where

import Control.Exception
import qualified Core.Interactor.GetUser as I
import Core.User
import Data.Integral.Exact
import qualified Data.Text as T
import qualified Network.Wai as Wai
import Web.Exception
import Web.Presenter.User

data Handle =
  Handle
    { hGetUserHandle :: I.Handle IO
    , hPresenterHandle :: RepBuilderHandle
    }

run :: Handle -> Wai.Application
run h request respond = do
  userIdent <-
    maybe (throwIO NotFoundException) pure $ parseUserId (Wai.pathInfo request)
  user <-
    maybe (throwIO NotFoundException) pure =<<
    I.run (hGetUserHandle h) userIdent
  respond $ presentUser (hPresenterHandle h) user Nothing

parseUserId :: [T.Text] -> Maybe UserId
parseUserId [s] = UserId <$> readExactIntegral (T.unpack s)
parseUserId _ = Nothing
