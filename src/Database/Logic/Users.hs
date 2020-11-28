{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ApplicativeDo #-}

module Database.Logic.Users
  ( createUser
  , selectUserById
  , selectUsers
  , selectUserAuthData
  , deleteUser
  , userColumns
  , optUserColumns
  ) where

import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Core.Authentication
import Core.Authentication.Impl
import Core.EntityId
import Core.Image
import qualified Core.Interactor.CreateUser as I
import Core.Interactor.DeleteUser as IDeleteUser
import Core.Pagination
import Core.User
import Data.Bifunctor
import Data.Foldable
import Data.Functor.Contravariant
import Data.Profunctor
import Data.Vector (Vector)
import {-# SOURCE #-} Database.Logic.Authors
import Database.Logic.Images
import Database.Logic.Pagination
import Database.Service.Columns
import Database.Service.Primitives
import qualified Hasql.Decoders as D
import qualified Hasql.Encoders as E
import qualified Hasql.TH as TH

createUser :: I.CreateUserCommand -> Transaction I.CreateUserResult
createUser cmd@I.CreateUserCommand {..} = do
  curAvatarId <-
    case cuAvatar of
      Just image -> Just <$> createImage image
      Nothing -> pure Nothing
  curUserId <- insertUser curAvatarId cmd
  pure I.CreateUserResult {curUserId, curAvatarId}

insertUser :: Maybe ImageId -> I.CreateUserCommand -> Transaction UserId
insertUser =
  curry . runStatement $
  dimap
    (\(optImageId, I.CreateUserCommand {..}) ->
       ( cuFirstName {-1-}
       , cuLastName {-2-}
       , getImageId <$> optImageId {-3-}
       , cuCreatedAt {-4-}
       , cuIsAdmin {-5-}
       , secretTokenHashBytes cuTokenHash {-6-}
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

selectUserById :: UserId -> Transaction (Maybe User)
selectUserById =
  runStatement $ statementWithColumns sql encoder userColumns D.rowMaybe True
  where
    sql = "select $COLUMNS from users where user_id = $1"
    encoder = getUserId >$< (E.param . E.nonNullable) E.int4

selectUsers :: PageSpec -> Transaction (Vector User)
selectUsers =
  runStatement $
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

-- | Allows all columns to be nullable (e.g. when using outer joins).
optUserColumns :: Columns (Maybe User)
optUserColumns = do
  optUserId <- fmap UserId <$> column usersTable "user_id"
  userFirstName <- column usersTable "first_name"
  optUserLastName <- column usersTable "last_name"
  userAvatarId <- fmap ImageId <$> column usersTable "avatar_id"
  optUserCreatedAt <- column usersTable "created_at"
  optUserIsAdmin <- column usersTable "is_admin"
  pure $
    case (optUserId, optUserLastName, optUserCreatedAt, optUserIsAdmin) of
      (Just userId, Just userLastName, Just userCreatedAt, Just userIsAdmin) ->
        Just User {..}
      _ -> Nothing

usersTable :: TableName
usersTable = "users"

selectUserAuthData :: UserId -> Transaction (Maybe UserAuthData)
selectUserAuthData uid = do
  hashAndIsAdmin <- getUserHashAndIsAdmin uid
  case hashAndIsAdmin of
    Nothing -> pure Nothing
    Just (authDataSecretTokenHash, authDataIsAdmin) -> do
      authDataAuthors <- selectAuthorsByUserId uid Nothing
      pure $ Just UserAuthData {..}

getUserHashAndIsAdmin :: UserId -> Transaction (Maybe (SecretTokenHash, Bool))
getUserHashAndIsAdmin =
  runStatement $
  dimap
    getUserId
    (fmap $ first SecretTokenHash)
    [TH.maybeStatement|
    select token_hash :: bytea, is_admin :: boolean
    from users
    where user_id = $1 :: integer
    |]

deleteUser :: UserId -> PageSpec -> Transaction (Either IDeleteUser.Failure ())
deleteUser uid defaultRange =
  runExceptT $ do
    optAvatarId <- ExceptT deleteUserReturningAvatarId
    case optAvatarId of
      Nothing -> throwE UnknownUser
      Just Nothing -> pure ()
      Just (Just avatarId) -> lift $ deleteImageIfNotReferenced avatarId
  where
    deleteUserReturningAvatarId = do
      authors <- selectAuthorsByUserId uid (Just defaultRange)
      if null authors
        then Right <$> deleteUserSt uid
        else pure $ dependencyFailure authors
    dependencyFailure =
      Left . DependentEntitiesPreventDeletion . map AuthorEntityId . toList

-- | Returns @Nothing@ if no users found; @Just Nothing@ if a user is
-- deleted and it did not have an avatar; @Just (Just avatarId)@
-- otherwise.
deleteUserSt :: UserId -> Transaction (Maybe (Maybe ImageId))
deleteUserSt =
  runStatement $
  dimap
    getUserId
    (fmap (fmap ImageId))
    [TH.maybeStatement|
    delete from users
    where user_id = $1 :: integer
    returning avatar_id :: integer?
    |]
