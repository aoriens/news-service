module Gateway.Users
  ( createUser
  , getUser
  , getUsers
  , getUserAuthData
  , deleteUser
  ) where

import Core.Authentication
import Core.Interactor.CreateUser
import Core.Pagination
import Core.User
import Data.Foldable
import Database as DB
import qualified Database.Users as DUsers

createUser :: DB.Handle -> CreateUserCommand -> IO CreateUserResult
createUser h = runTransactionRW h . DUsers.createUser

getUser :: DB.Handle -> UserId -> IO (Maybe User)
getUser h = runTransaction h . statement DUsers.selectUserById

getUsers :: DB.Handle -> Page -> IO [User]
getUsers h page =
  toList <$> runTransaction h (statement DUsers.selectUsers page)

getUserAuthData :: DB.Handle -> UserId -> IO (Maybe (SecretTokenHash, IsAdmin))
getUserAuthData h = runTransaction h . statement DUsers.selectUserAuthData

deleteUser :: DB.Handle -> UserId -> IO ()
deleteUser h = runTransactionRW h . statement DUsers.deleteUser
