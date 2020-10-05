{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ApplicativeDo #-}

module Database.Users
  ( createUser
  , selectUserById
  , selectUsers
  , selectUserAuthData
  , deleteUser
  , userColumns
  ) where

import Control.Arrow
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import qualified Core.Authentication as Auth
import Core.EntityId
import Core.Image
import qualified Core.Interactor.CreateUser as I
import Core.Interactor.DeleteUser as IDeleteUser
import Core.Pagination
import Core.User
import Data.Foldable
import Data.Functor.Contravariant
import Data.Profunctor
import Data.Vector (Vector)
import Database
import {-# SOURCE #-} Database.Authors
import Database.Columns
import Database.Images
import Database.Pagination
import qualified Hasql.Decoders as D
import qualified Hasql.Encoders as E
import qualified Hasql.TH as TH

createUser :: I.CreateUserCommand -> Transaction I.CreateUserResult
createUser cmd@I.CreateUserCommand {..} = do
  optAvatarId <-
    case cuAvatar of
      Just image -> Just <$> createImage image
      Nothing -> pure Nothing
  userId <- statement insertUser (optAvatarId, cmd)
  pure I.CreateUserResult {curUserId = userId, curAvatarId = optAvatarId}

insertUser :: Statement (Maybe ImageId, I.CreateUserCommand) UserId
insertUser =
  dimap
    (\(optImageId, I.CreateUserCommand {..}) ->
       ( cuFirstName {-1-}
       , cuLastName {-2-}
       , getImageId <$> optImageId {-3-}
       , cuCreatedAt {-4-}
       , cuIsAdmin {-5-}
       , Auth.secretTokenHashBytes cuTokenHash {-6-}
        ))
    UserId
    [TH.singletonStatement|
    insert into users (
      first_name,
      last_name,
      avatar_id,
      created_at,
      is_admin,
      token_hash
    ) values (
      $1 :: varchar?,
      $2 :: varchar,
      $3 :: integer?,
      $4 :: timestamptz,
      $5 :: boolean,
      $6 :: bytea
    ) returning user_id :: integer
    |]

selectUserById :: Statement UserId (Maybe User)
selectUserById = statementWithColumns sql encoder userColumns D.rowMaybe True
  where
    sql = "select $COLUMNS from users where user_id = $1"
    encoder = getUserId >$< (E.param . E.nonNullable) E.int4

selectUsers :: Statement PageSpec (Vector User)
selectUsers =
  statementWithColumns
    "select $COLUMNS from users limit $1 offset $2"
    pageToLimitOffsetEncoder
    userColumns
    D.rowVector
    True

userColumns :: Columns User
userColumns = do
  userId <- UserId <$> column usersTable "user_id"
  userFirstName <- column usersTable "first_name"
  userLastName <- column usersTable "last_name"
  userAvatarId <- fmap ImageId <$> column usersTable "avatar_id"
  userCreatedAt <- column usersTable "created_at"
  userIsAdmin <- column usersTable "is_admin"
  pure User {..}

usersTable :: TableName
usersTable = "users"

selectUserAuthData ::
     Statement UserId (Maybe (Auth.SecretTokenHash, Auth.IsAdmin))
selectUserAuthData =
  dimap
    getUserId
    (fmap $ first Auth.SecretTokenHash)
    [TH.maybeStatement|
    select token_hash :: bytea, is_admin :: boolean
    from users
    where user_id = $1 :: integer
    |]

deleteUser :: UserId -> PageSpec -> Session (Either IDeleteUser.Failure ())
deleteUser uid defaultRange =
  runExceptT $ do
    optAvatarId <- ExceptT deleteUserReturningAvatarId
    case optAvatarId of
      Nothing -> throwE UnknownUser
      Just Nothing -> pure ()
      Just (Just avatarId) -> lift $ deleteImageIfNotReferenced avatarId
  where
    deleteUserReturningAvatarId =
      transactionRW $ do
        authors <- statement selectAuthorsByUserId (uid, defaultRange)
        if null authors
          then Right <$> statement deleteUserSt uid
          else pure $ dependencyFailure authors
    dependencyFailure =
      Left . DependentEntitiesPreventDeletion . map AuthorEntityId . toList

-- | Returns @Nothing@ if no users found; @Just Nothing@ if a user is
-- deleted and it did not have an avatar; @Just (Just avatarId)@
-- otherwise.
deleteUserSt :: Statement UserId (Maybe (Maybe ImageId))
deleteUserSt =
  dimap
    getUserId
    (fmap (fmap ImageId))
    [TH.maybeStatement|
    delete from users
    where user_id = $1 :: integer
    returning avatar_id :: integer?
    |]
