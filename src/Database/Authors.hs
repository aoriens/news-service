{-# LANGUAGE QuasiQuotes #-}

module Database.Authors
  ( createAuthor
  ) where

import Control.Arrow
import Core.Author
import qualified Core.Interactor.CreateAuthor as I
import Core.User
import Data.Profunctor
import qualified Data.Text as T
import Database
import Database.Users
import Hasql.Statement as S
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

insertAuthor :: S.Statement (UserId, T.Text) AuthorId
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
