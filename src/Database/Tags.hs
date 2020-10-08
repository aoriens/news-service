{-# LANGUAGE QuasiQuotes #-}

module Database.Tags
  ( findTagByName
  , findTagById
  , createTagNamed
  ) where

import Core.Tag
import Data.Profunctor
import qualified Data.Text as T
import Database
import qualified Hasql.TH as H

findTagByName :: Statement T.Text (Maybe Tag)
findTagByName =
  rmap
    (fmap $ \(tid, tagName) -> Tag {tagId = TagId tid, tagName})
    [H.maybeStatement|
      select tag_id :: integer, name :: varchar
      from tags
      where name = $1 :: varchar
      limit 1
    |]

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
findTagById =
  dimap
    getTagId
    (fmap $ \(tid, tagName) -> Tag {tagId = TagId tid, tagName})
    [H.maybeStatement|
      select tag_id :: integer, name :: varchar
      from tags
      where tag_id = $1 :: integer
    |]
