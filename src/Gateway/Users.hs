module Gateway.Users
  ( createUser
  , getUser
  , getUsers
  , getUserAuthData
  , deleteUser
  ) where

import Control.Exception.Sync
import Core.Authentication
import Core.EntityId
import Core.Interactor.CreateUser
import Core.Interactor.DeleteUser as IDeleteUser
import Core.Pagination
import Core.User
import Data.Foldable
import Database as DB
import Database.Authors
import qualified Database.Users as DUsers
import qualified PostgreSQL.ErrorCodes as PE

createUser :: DB.Handle -> CreateUserCommand -> IO CreateUserResult
createUser h = runTransactionRW h . DUsers.createUser

getUser :: DB.Handle -> UserId -> IO (Maybe User)
getUser h = runTransaction h . statement DUsers.selectUserById

getUsers :: DB.Handle -> Page -> IO [User]
getUsers h page =
  toList <$> runTransaction h (statement DUsers.selectUsers page)

getUserAuthData :: DB.Handle -> UserId -> IO (Maybe (SecretTokenHash, IsAdmin))
getUserAuthData h = runTransaction h . statement DUsers.selectUserAuthData

deleteUser :: DB.Handle -> UserId -> Page -> IO (Either IDeleteUser.Failure ())
deleteUser h uid defaultRange =
  catchJustS'
    (isDatabaseResultErrorWithCode PE.foreign_key_violation)
    deleteFast
    deleteWithCheck
  where
    deleteFast = Right <$> runTransactionRW h (statement DUsers.deleteUser uid)
    deleteWithCheck =
      runTransactionRW h $ do
        authors <- statement selectAuthorsByUserId (uid, defaultRange)
        if null authors
          then Right <$> statement DUsers.deleteUser uid
          else pure .
               Left . DependentEntitiesPreventDeletion . map AuthorEntityId $
               toList authors
