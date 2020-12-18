{-# LANGUAGE QuasiQuotes #-}

module Database.Logic.News.GetDraftAuthor
  ( getDraftAuthor
  ) where

import Core.Author
import Core.Deletable
import Core.News
import Data.Profunctor
import Database.Service.Primitives
import qualified Hasql.TH as TH

getDraftAuthor :: DraftId -> Transaction (Maybe (Deletable AuthorId))
getDraftAuthor =
  runStatement $
  dimap
    getDraftId
    (fmap (deletableFromMaybe . fmap AuthorId))
    [TH.maybeStatement|
      select author_id :: integer?
      from drafts
      where news_version_id = $1 :: integer
    |]
