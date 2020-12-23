{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ApplicativeDo #-}

module Database.Logic.Tags
  ( findTagNamed
  , getTag
  , tagExists
  , createTagNamed
  , getTags
  , deleteTag
  , setTagName
  , tagColumns
  ) where

import qualified Core.Interactor.UpdateTag as IUpdateTag
import Core.Pagination
import Core.Tag
import Data.Bifunctor
import Data.Bool
import Data.Foldable
import Data.Functor.Contravariant
import Data.Profunctor
import qualified Data.Text as T
import Database.Logic.Pagination
import Database.Service.Columns
import Database.Service.Primitives
import qualified Hasql.Decoders as D
import qualified Hasql.Encoders as E
import qualified Hasql.TH as H

findTagNamed :: T.Text -> Transaction (Maybe Tag)
findTagNamed =
  runStatement $ statementWithColumns sql encoder tagColumns D.rowMaybe True
  where
    sql = "select $COLUMNS from tags where name = $1"
    encoder = E.param (E.nonNullable E.text)

createTagNamed :: T.Text -> Transaction Tag
createTagNamed tagName = do
  tagId <- createTagRow tagName
  pure Tag {tagName, tagId}

createTagRow :: T.Text -> Transaction TagId
createTagRow =
  runStatement $
  rmap
    TagId
    [H.singletonStatement|
      insert into tags (name) values (
        $1 :: varchar
      ) returning tag_id :: integer
    |]

setTagName ::
     TagId -> T.Text -> Transaction (Either IUpdateTag.SetTagNameFailure ())
setTagName =
  curry . runStatement $
  dimap
    (first getTagId)
    (bool (Left IUpdateTag.STNUnknownTagId) (Right ()) . (> 0))
    [H.rowsAffectedStatement|
      update tags
      set name = $2 :: varchar
      where tag_id = $1 :: integer
    |]

getTag :: TagId -> Transaction (Maybe Tag)
getTag =
  runStatement $ statementWithColumns sql encoder tagColumns D.rowMaybe True
  where
    sql = "select $COLUMNS from tags where tag_id = $1"
    encoder = getTagId >$< E.param (E.nonNullable E.int4)

tagExists :: TagId -> Transaction Bool
tagExists =
  runStatement $
  lmap
    getTagId
    [H.singletonStatement|
      select exists (
        select 1 from tags where tag_id = $1 :: integer
      ) :: bool
    |]

deleteTag :: TagId -> Transaction Bool
deleteTag =
  runStatement $
  dimap
    getTagId
    (> 0)
    [H.rowsAffectedStatement|
      delete from tags
      where tag_id = $1 :: integer
    |]

getTags :: PageSpec -> Transaction [Tag]
getTags =
  runStatement $
  statementWithColumns sql pageToLimitOffsetEncoder tagColumns decoder True
  where
    sql = "select $COLUMNS from tags limit $1 offset $2"
    decoder = fmap toList . D.rowVector

tagColumns :: Columns Tag
tagColumns = do
  tagId <- TagId <$> column tagsTable "tag_id"
  tagName <- column tagsTable "name"
  pure Tag {..}

tagsTable :: TableName
tagsTable = "tags"
