{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

module Gateway.Users
  ( createUser
  ) where

import qualified Core.Interactor.CreateUser as I
import Data.Profunctor
import qualified Data.Text as T
import Database as DB
import qualified Hasql.Statement as S
import qualified Hasql.TH as TH

createUser :: DB.Handle -> I.CreateUserCommand -> IO I.CreateUserResult
createUser h cmd@I.CreateUserCommand {..} =
  DB.runTransactionRW h $ do
    optAvatarId <-
      case cuAvatar of
        Just image -> do
          DB.statement createMimeTypeIfNotFound (I.imageContentType image)
          Just <$> DB.statement createImage image
        Nothing -> pure Nothing
    userId <- DB.statement createUserSt (optAvatarId, cmd)
    pure I.CreateUserResult {curUserId = userId, curAvatarId = optAvatarId}

createMimeTypeIfNotFound :: S.Statement T.Text ()
createMimeTypeIfNotFound =
  [TH.resultlessStatement|
    insert into mime_types (value) values ($1 :: varchar) on conflict do nothing
  |]

createImage :: S.Statement I.Image I.ImageId
createImage =
  dimap
    (\I.Image {..} -> (imageData, imageContentType))
    I.ImageId
    [TH.singletonStatement|
    insert into images (content, mime_type_id)
    values (
      $1 :: bytea,
      (select mime_type_id from mime_types where value = $2 :: varchar)
    ) returning image_id :: integer
    |]

createUserSt :: S.Statement (Maybe I.ImageId, I.CreateUserCommand) I.UserId
createUserSt =
  dimap
    (\(optImageId, I.CreateUserCommand {..}) ->
       ( cuFirstName {-1-}
       , cuLastName {-2-}
       , I.getImageId <$> optImageId {-3-}
       , cuCreatedAt {-4-}
       , cuIsAdmin {-5-}
       , cuTokenHash {-6-}
        ))
    I.UserId
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
