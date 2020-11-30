{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ApplicativeDo #-}

module Database.Logic.Comments
  ( createComment
  , getComment
  , getCommentsForNews
  ) where

import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Core.Comment
import Core.EntityId
import qualified Core.Interactor.CreateComment as ICreateComment
import qualified Core.Interactor.GetCommentsForNews as IGetCommentsForNews
import Core.News
import Core.Pagination
import Core.User
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
    optUser <- maybe (pure Nothing) (fmap Just . loadUserIdOrFail) optUserId
    commentId <- lift $ insertComment text optUserId newsId' createdAt
    pure
      Comment
        { commentText = text
        , commentAuthor = maybe AnonymousCommentAuthor UserCommentAuthor optUser
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
      lift (selectUserById userId') >>=
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

commentColumns :: Columns Comment
commentColumns = do
  commentId <- CommentId <$> column table "comment_id"
  commentNewsId <- NewsId <$> column table "news_id"
  commentAuthor <-
    maybe AnonymousCommentAuthor UserCommentAuthor <$> optUserColumns
  commentCreatedAt <- column table "created_at"
  commentText <- column table "text"
  pure Comment {..}

table :: TableName
table = "comments"
