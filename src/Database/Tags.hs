{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ApplicativeDo #-}

module Database.Tags
  ( findTagByName
  , findTagById
  , createTagNamed
  , getTags
  ) where

import Core.Pagination
import Core.Tag
import Data.Foldable
import Data.Functor.Contravariant
import Data.Profunctor
import qualified Data.Text as T
import Database
import Database.Columns
import Database.Pagination
import qualified Hasql.Decoders as D
import qualified Hasql.Encoders as E
import qualified Hasql.TH as H

findTagByName :: Statement T.Text (Maybe Tag)
findTagByName = statementWithColumns sql encoder tagColumns D.rowMaybe True
  where
    sql = "select $COLUMNS from tags where name = $1"
    encoder = E.param (E.nonNullable E.text)

createTagNamed :: T.Text -> Transaction Tag
createTagNamed tagName = do
  tagId <- statement createTagNamedSt tagName
  pure Tag {tagName, tagId}

createTagNamedSt :: Statement T.Text TagId
createTagNamedSt =
  rmap
    TagId
    [H.singletonStatement|
      insert into tags (name) values (
        $1 :: varchar
      ) returning tag_id :: integer
    |]

findTagById :: Statement TagId (Maybe Tag)
findTagById = statementWithColumns sql encoder tagColumns D.rowMaybe True
  where
    sql = "select $COLUMNS from tags where tag_id = $1"
    encoder = getTagId >$< E.param (E.nonNullable E.int4)

getTags :: Statement PageSpec [Tag]
getTags =
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
