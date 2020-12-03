{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ApplicativeDo #-}

module Database.Logic.Authors
  ( createAuthor
  , selectAuthors
  , selectAuthorById
  , updateAuthor
  , selectAuthorsByUserId
  , selectAuthorIdByUserIdIfExactlyOne
  , deleteAuthorById
  , authorColumns
  , deletableAuthorColumns
  ) where

import Control.Arrow
import Core.Author
import Core.Deletable
import qualified Core.Interactor.CreateAuthor as I
import Core.Pagination
import Core.User
import Data.Foldable
import Data.Functor.Contravariant
import Data.Int
import Data.Profunctor
import qualified Data.Text as T
import Data.Tuple
import Database.Logic.Pagination
import Database.Logic.Users
import Database.Service.Columns
import Database.Service.Primitives
import qualified Hasql.Decoders as D
import qualified Hasql.Encoders as E
import Hasql.TH as TH

createAuthor :: UserId -> T.Text -> Transaction (Either I.Failure Author)
createAuthor uid description = do
  optUser <- getExistingUser uid
  case optUser of
    Nothing -> pure $ Left I.UnknownUserId
    Just user -> do
      aid <- insertAuthor uid description
      pure $
        Right
          Author
            { authorId = aid
            , authorUser = Existing user
            , authorDescription = description
            }

insertAuthor :: UserId -> T.Text -> Transaction AuthorId
insertAuthor =
  curry . runStatement $
  dimap
    (first getUserId)
    AuthorId
    [TH.singletonStatement|
    insert into authors (user_id, description) values (
        $1 :: integer,
        $2 :: varchar
    ) returning author_id :: integer
    |]

selectAuthors :: PageSpec -> Transaction [Author]
selectAuthors =
  runStatement $
  statementWithColumns
    "select $COLUMNS from authors join users using (user_id) limit $1 offset $2"
    pageToLimitOffsetEncoder
    authorColumns
    (fmap toList . D.rowVector)
    True

selectAuthorById :: AuthorId -> Transaction (Maybe Author)
selectAuthorById =
  runStatement $
  statementWithColumns
    "select $COLUMNS from authors join users using (user_id) where author_id = $1"
    (getAuthorId >$< E.param (E.nonNullable E.int4))
    authorColumns
    D.rowMaybe
    True

updateAuthor :: AuthorId -> T.Text -> Transaction ()
updateAuthor =
  curry . runStatement $
  lmap
    (swap . first getAuthorId)
    [TH.resultlessStatement|
       update authors
       set description = $1 :: text
       where author_id = $2 :: integer
    |]

-- | Author columns. This may be used with inner joins with authors
-- table.
authorColumns :: Columns Author
authorColumns = do
  authorUser <- userColumns
  authorId <- AuthorId <$> column authorsTable "author_id"
  authorDescription <- column authorsTable "description"
  pure Author {..}

-- | An author may be considered 'Deleted', if no such author found,
-- e.g. at least one field declared nonnull is null in an outer join.
deletableAuthorColumns :: Columns (Deletable Author)
deletableAuthorColumns = do
  optUser <- optUserColumns
  optId <- fmap AuthorId <$> column authorsTable "author_id"
  optDescription <- column authorsTable "description"
  pure $
    case (optId, optUser, optDescription) of
      (Just authorId, Just authorUser, Just authorDescription) ->
        Existing Author {..}
      _ -> Deleted

authorsTable :: TableName
authorsTable = "authors"

selectAuthorsByUserId :: UserId -> Maybe PageSpec -> Transaction [AuthorId]
selectAuthorsByUserId =
  curry . runStatement $
  dimap
    (\(uid, page) ->
       ( getUserId uid
       , getPageLimit . pageLimit <$> page
       , getPageOffset . pageOffset <$> page))
    (map AuthorId . toList)
    [TH.vectorStatement|
       select author_id :: integer
       from authors join users using (user_id)
       where user_id = $1 :: integer
       limit $2 :: integer? offset $3 :: integer?
    |]

selectAuthorIdByUserIdIfExactlyOne :: UserId -> Transaction (Maybe AuthorId)
selectAuthorIdByUserIdIfExactlyOne uid = do
  authorIds <- selectAuthorsByUserId uid upTo2
  pure $
    case authorIds of
      [aid] -> Just aid
      _ -> Nothing
  where
    upTo2 = Just PageSpec {pageLimit = PageLimit 2, pageOffset = PageOffset 0}

deleteAuthorById :: AuthorId -> Transaction Int64
deleteAuthorById =
  runStatement $
  lmap
    getAuthorId
    [TH.rowsAffectedStatement|
       delete from authors
       where author_id = $1 :: integer
    |]
