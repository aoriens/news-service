{-# LANGUAGE QuasiQuotes #-}

module Database.Authors
  ( createAuthor
  , selectAuthors
  ) where

import Control.Arrow
import Core.Author
import Core.Image
import qualified Core.Interactor.CreateAuthor as I
import Core.Pagination
import Core.User
import Data.Int
import Data.Profunctor
import qualified Data.Text as T
import Data.Time
import Data.Vector (Vector)
import Database
import Database.Users
import Hasql.TH as TH

createAuthor :: UserId -> T.Text -> Transaction (Either I.Failure Author)
createAuthor uid description = do
  optUser <- statement selectUserById uid
  case optUser of
    Nothing -> pure $ Left I.UnknownUserId
    Just user -> do
      aid <- statement insertAuthor (uid, description)
      pure $
        Right
          Author
            {authorId = aid, authorUser = user, authorDescription = description}

insertAuthor :: Statement (UserId, T.Text) AuthorId
insertAuthor =
  dimap
    (first getUserId)
    AuthorId
    [TH.singletonStatement|
    insert into authors (user_id, description) values (
        $1 :: integer,
        $2 :: varchar
    ) returning author_id :: integer
    |]

selectAuthors :: Statement Page (Vector Author)
selectAuthors =
  dimap
    (\Page {..} -> (getPageLimit pageLimit, getPageOffset pageOffset))
    (fmap decodeAuthor)
    [TH.vectorStatement|
    select authors.author_id :: integer,
           authors.description :: varchar,
           users.user_id :: integer,
           users.first_name :: varchar?,
           users.last_name :: varchar,
           users.avatar_id :: integer?,
           users.created_at :: timestamptz,
           users.is_admin :: boolean
    from authors join users using (user_id)
    limit $1 :: integer offset $2 :: integer
    |]

decodeAuthor ::
     (Int32, T.Text, Int32, Maybe T.Text, T.Text, Maybe Int32, UTCTime, Bool)
  -> Author
decodeAuthor (authorId, authorDescription, userId, userFirstName, userLastName, userAvatarId, userCreatedAt, userIsAdmin) =
  Author
    { authorId = AuthorId authorId
    , authorUser =
        User
          {userId = UserId userId, userAvatarId = ImageId <$> userAvatarId, ..}
    , ..
    }
