module Gateway.Users
  ( createUser
  , getUser
  , getUsers
  , getUserAuthData
  , deleteUser
  ) where

import Core.Authentication.Impl
import Core.Interactor.CreateUser
import Core.Interactor.DeleteUser as IDeleteUser
import Core.Pagination
import Core.User
import Data.Foldable
import Database as DB
import qualified Database.Users as DUsers

createUser :: DB.Handle -> CreateUserCommand -> IO CreateUserResult
createUser h = runTransactionRW h . DUsers.createUser

getUser :: DB.Handle -> UserId -> IO (Maybe User)
getUser h = runTransactionRO h . DUsers.selectUserById

getUsers :: DB.Handle -> PageSpec -> IO [User]
getUsers h page = toList <$> runTransactionRO h (DUsers.selectUsers page)

getUserAuthData :: DB.Handle -> UserId -> IO (Maybe UserAuthData)
getUserAuthData h = runTransactionRO h . DUsers.selectUserAuthData

deleteUser ::
     DB.Handle -> UserId -> PageSpec -> IO (Either IDeleteUser.Failure ())
deleteUser h uid defaultRange =
  runSession h $ DUsers.deleteUser uid defaultRange
