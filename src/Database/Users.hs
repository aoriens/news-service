{-# LANGUAGE QuasiQuotes #-}

module Database.Users
  ( createUser
  , selectUserById
  , selectUsers
  , selectUserAuthData
  , deleteUser
  ) where

import Control.Arrow
import qualified Core.Authentication as Auth
import Core.Image
import qualified Core.Interactor.CreateUser as I
import Core.Pagination
import Core.User
import Data.Int
import Data.Profunctor
import Data.Text (Text)
import Data.Time
import Data.Vector (Vector)
import Database
import Database.Images
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
selectUserById =
  dimap
    getUserId
    (fmap decodeUser)
    [TH.maybeStatement|
    select user_id :: integer,
           first_name :: varchar?,
           last_name :: varchar,
           avatar_id :: integer?,
           created_at :: timestamptz,
           is_admin :: boolean
    from users
    where user_id = $1 :: integer
    |]

selectUsers :: Statement Page (Vector User)
selectUsers =
  dimap
    (\Page {..} -> (getPageLimit pageLimit, getPageOffset pageOffset))
    (fmap decodeUser)
    [TH.vectorStatement|
    select user_id :: integer,
           first_name :: varchar?,
           last_name :: varchar,
           avatar_id :: integer?,
           created_at :: timestamptz,
           is_admin :: boolean
    from users
    limit $1 :: integer offset $2 :: integer
    |]

decodeUser :: (Int32, Maybe Text, Text, Maybe Int32, UTCTime, Bool) -> User
decodeUser (userId, userFirstName, userLastName, userAvatarId, userCreatedAt, userIsAdmin) =
  User {userId = UserId userId, userAvatarId = ImageId <$> userAvatarId, ..}

selectUserAuthData ::
     Statement UserId (Maybe (Auth.SecretTokenHash, Auth.IsAdmin))
selectUserAuthData =
  dimap
    getUserId
    (fmap (Auth.SecretTokenHash *** Auth.IsAdmin))
    [TH.maybeStatement|
    select token_hash :: bytea, is_admin :: boolean
    from users
    where user_id = $1 :: integer
    |]

deleteUser :: Statement UserId ()
deleteUser =
  dimap
    getUserId
    id
    [TH.resultlessStatement|
    delete from users
    where user_id = $1 :: integer
    |]
