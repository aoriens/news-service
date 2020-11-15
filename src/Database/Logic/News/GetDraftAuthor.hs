{-# LANGUAGE QuasiQuotes #-}

module Database.Logic.News.GetDraftAuthor
  ( getDraftAuthor
  ) where

import Core.Author
import qualified Core.Interactor.PublishDraft as IPublishDraft
import Core.News
import Data.Profunctor
import Database.Service.Primitives
import qualified Hasql.TH as TH

getDraftAuthor ::
     NewsVersionId -> Transaction (Either IPublishDraft.GatewayFailure AuthorId)
getDraftAuthor =
  runStatement $
  dimap
    getNewsVersionId
    (maybe (Left IPublishDraft.UnknownDraftId) (Right . AuthorId))
    [TH.maybeStatement|
      select author_id :: integer
      from drafts
      where news_version_id = $1 :: integer
    |]
