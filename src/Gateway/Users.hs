module Gateway.Users
  ( createUser
  , getUser
  , getUsers
  , getUserAuthData
  , deleteUser
  ) where

import qualified Core.Authentication as Auth
import qualified Core.Interactor.CreateUser as I
import Core.Pagination
import Core.User
import Data.Foldable
import qualified Database as DB
import qualified Database.Users as DUsers

createUser :: DB.Handle -> I.CreateUserCommand -> IO I.CreateUserResult
createUser h = DB.runTransactionRW h . DUsers.createUser

getUser :: DB.Handle -> UserId -> IO (Maybe User)
getUser h = DB.runTransaction h . DB.statement DUsers.selectUserById

getUsers :: DB.Handle -> Page -> IO [User]
getUsers h page =
  toList <$> DB.runTransaction h (DB.statement DUsers.selectUsers page)

getUserAuthData ::
     DB.Handle -> UserId -> IO (Maybe (Auth.SecretTokenHash, Auth.IsAdmin))
getUserAuthData h = DB.runTransaction h . DB.statement DUsers.selectUserAuthData

deleteUser :: DB.Handle -> UserId -> IO ()
deleteUser h = DB.runTransactionRW h . DB.statement DUsers.deleteUser
