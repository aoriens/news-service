{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE LambdaCase #-}

module Database.Logic.Comments
  ( createComment
  , getComment
  , getCommentsForNews
  , getCommentAuthor
  , deleteComment
  ) where

import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Core.Comment
import Core.Deletable
import Core.EntityId
import qualified Core.Interactor.CreateComment as ICreateComment
import qualified Core.Interactor.GetCommentsForNews as IGetCommentsForNews
import Core.News
import Core.Pagination
import Core.User
import Data.Functor
import Data.Profunctor
import qualified Data.Text as T
import Data.Time
import Database.Logic.News.Exists
import Database.Logic.Pagination
import Database.Logic.Users
import Database.Service.Columns
import Database.Service.Primitives
import qualified Database.Service.SQLBuilder as Sql
import qualified Hasql.Decoders as D
import qualified Hasql.TH as TH

createComment ::
     T.Text
  -> Maybe UserId
  -> NewsId
  -> UTCTime
  -> Transaction (Either ICreateComment.GatewayFailure Comment)
createComment text optUserId newsId' createdAt =
  runExceptT $ do
    failIfNewsDoesNotExist
    commentAuthor <-
      case optUserId of
        Nothing -> pure AnonymousCommentAuthor
        Just userId -> UserCommentAuthor <$> loadUserIdOrFail userId
    commentId <- lift $ insertComment text optUserId newsId' createdAt
    pure
      Comment
        { commentText = text
        , commentAuthor
        , commentCreatedAt = createdAt
        , commentId
        , commentNewsId = newsId'
        }
  where
    failIfNewsDoesNotExist = do
      newsExists' <- lift $ newsExists newsId'
      unless newsExists' $
        throwE $ ICreateComment.GUnknownEntityId $ toEntityId newsId'
    loadUserIdOrFail userId' =
      lift (getExistingUser userId') >>=
      maybe (throwE $ ICreateComment.GUnknownEntityId $ toEntityId userId') pure

insertComment ::
     T.Text -> Maybe UserId -> NewsId -> UTCTime -> Transaction CommentId
insertComment text optUserId newsId' createdAt = runStatement statement args
  where
    args = (getNewsId newsId', getUserId <$> optUserId, text, createdAt)
    statement =
      rmap
        CommentId
        [TH.singletonStatement|
          insert into comments (
            news_id,
            user_id,
            text,
            created_at
          ) values (
            $1 :: integer,
            $2 :: integer?,
            $3 :: varchar,
            $4 :: timestamptz
          ) returning comment_id :: integer
        |]

getComment :: CommentId -> Transaction (Maybe Comment)
getComment (CommentId commentId) =
  runStatementWithColumns sql commentColumns D.rowMaybe True
  where
    sql =
      Sql.text
        [TH.uncheckedSql|
           select $COLUMNS
           from comments
                left join users using (user_id)
           where comment_id =
        |] <>
      Sql.param commentId

getCommentsForNews ::
     NewsId
  -> PageSpec
  -> Transaction (Either IGetCommentsForNews.GatewayFailure [Comment])
getCommentsForNews newsId pageSpec = do
  comments <- getCommentsForNewsUnchecked newsId pageSpec
  if null comments
    then do
      newsExists' <- newsExists newsId
      if newsExists'
        then pure $ Right comments
        else pure $ Left IGetCommentsForNews.GUnknownNewsId
    else pure $ Right comments

getCommentsForNewsUnchecked :: NewsId -> PageSpec -> Transaction [Comment]
getCommentsForNewsUnchecked (NewsId newsId) pageSpec =
  runStatementWithColumns sql commentColumns D.rowList True
  where
    sql =
      Sql.text
        [TH.uncheckedSql|
           select $COLUMNS
           from comments
                left join users using (user_id)
           where news_id =
        |] <>
      Sql.param newsId <>
      "order by comment_id" <> limitOffsetClauseWithPageSpec pageSpec

getCommentAuthor :: CommentId -> Transaction (Maybe (CommentAuthor UserId))
getCommentAuthor =
  runStatement $
  dimap
    getCommentId
    (fmap decodeRow)
    [TH.maybeStatement|
      select comments.user_id :: integer?, users.is_deleted :: boolean?
      from comments
           left join users using (user_id)
      where comment_id = $1 :: integer
    |]
  where
    decodeRow (Nothing, _) = AnonymousCommentAuthor
    decodeRow (Just userId, Just False) = UserCommentAuthor $ UserId userId
    decodeRow (Just _, _) = DeletedCommentAuthor

deleteComment :: CommentId -> Transaction Bool
deleteComment =
  runStatement $
  dimap
    getCommentId
    (> 0)
    [TH.rowsAffectedStatement|
      delete from comments
      where comment_id = $1 :: integer
    |]

commentColumns :: Columns Comment
commentColumns = do
  commentId <- CommentId <$> column table "comment_id"
  commentNewsId <- NewsId <$> column table "news_id"
  commentAuthor <-
    optUserColumns <&> \case
      Nothing -> AnonymousCommentAuthor
      Just Deleted -> DeletedCommentAuthor
      Just (Existing user) -> UserCommentAuthor user
  commentCreatedAt <- column table "created_at"
  commentText <- column table "text"
  pure Comment {..}

table :: TableName
table = "comments"
