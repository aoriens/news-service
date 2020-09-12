{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ApplicativeDo #-}

module Database.Authors
  ( createAuthor
  , selectAuthors
  , selectAuthorById
  , selectAuthorsByUserId
  ) where

import Control.Arrow
import Core.Author
import qualified Core.Interactor.CreateAuthor as I
import Core.Pagination
import Core.User
import Data.Functor.Contravariant
import Data.Profunctor
import qualified Data.Text as T
import Data.Vector (Vector)
import Database
import Database.Columns
import Database.Pagination
import Database.Users
import qualified Hasql.Decoders as D
import qualified Hasql.Encoders as E
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

selectAuthors :: Statement PageSpec (Vector Author)
selectAuthors =
  selectColumns
    D.rowVector
    authorColumns
    "from authors join users using (user_id) limit $1 offset $2"
    pageToLimitOffsetEncoder
    True

selectAuthorById :: Statement AuthorId (Maybe Author)
selectAuthorById =
  selectColumns
    D.rowMaybe
    authorColumns
    "from authors join users using (user_id) where author_id = $1"
    (getAuthorId >$< E.param (E.nonNullable E.int4))
    True

authorColumns :: Columns Author
authorColumns = do
  authorUser <- userColumns
  authorId <- AuthorId <$> column authorsTable "author_id"
  authorDescription <- column authorsTable "description"
  pure Author {..}

authorsTable :: TableName
authorsTable = "authors"

selectAuthorsByUserId :: Statement (UserId, PageSpec) (Vector AuthorId)
selectAuthorsByUserId =
  dimap
    (\(uid, page) ->
       ( getUserId uid
       , getPageLimit $ pageLimit page
       , getPageOffset $ pageOffset page))
    (fmap AuthorId)
    [TH.vectorStatement|
       select author_id :: integer
       from authors join users using (user_id)
       where user_id = $1 :: integer
       limit $2 :: integer offset $3 :: integer
    |]
