{-# LANGUAGE QuasiQuotes #-}

module Database.Logic.News.GetDraftAuthor
  ( getDraftAuthor
  ) where

import Core.Author
import Core.News
import Data.Profunctor
import Database.Service.Primitives
import qualified Hasql.TH as TH

getDraftAuthor :: NewsVersionId -> Transaction (Maybe AuthorId)
getDraftAuthor =
  runStatement $
  dimap
    getNewsVersionId
    (fmap AuthorId)
    [TH.maybeStatement|
      select author_id :: integer
      from drafts
      where news_version_id = $1 :: integer
    |]
