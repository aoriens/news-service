{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ApplicativeDo #-}

module Database.Logic.Comments
  ( createComment
  , getComment
  ) where

import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Core.Comment
import Core.EntityId
import qualified Core.Interactor.CreateComment as ICreateComment
import Core.News
import Core.User
import Data.Profunctor
import qualified Data.Text as T
import Data.Time
import Database.Logic.News.Exists
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
        , commentAuthor = optUser
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

commentColumns :: Columns Comment
commentColumns = do
  commentId <- CommentId <$> column table "comment_id"
  commentNewsId <- NewsId <$> column table "news_id"
  commentAuthor <- optUserColumns
  commentCreatedAt <- column table "created_at"
  commentText <- column table "text"
  pure Comment {..}

table :: TableName
table = "comments"
