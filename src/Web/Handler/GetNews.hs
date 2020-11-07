{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ApplicativeDo #-}

module Web.Handler.GetNews
  ( Handle(..)
  , run
  ) where

import Control.Applicative
import qualified Core.Interactor.GetNews as I
import Core.News
import Core.Pagination
import qualified Data.Aeson as A
import qualified Data.ByteString.Builder as BB
import Web.Application
import Web.QueryParameter
import Web.QueryParameter.PageQuery

data Handle =
  Handle
    { hGetNewsHandle :: I.Handle IO
    , hJSONEncode :: forall a. A.ToJSON a =>
                                 a -> BB.Builder
    , hPresenter :: [News] -> Response
    }

run :: Handle -> Application
run Handle {..} request respond = do
  (pageQuery, newsFilter) <-
    parseQueryM (requestQueryString request) parseParams
  news <- I.getNews hGetNewsHandle newsFilter pageQuery
  respond $ hPresenter news

parseParams :: QueryParser (PageSpecQuery, I.NewsFilter)
parseParams = liftA2 (,) parsePageQuery parseNewsFilter

parseNewsFilter :: QueryParser I.NewsFilter
parseNewsFilter = do
  optDay <- lookupQueryParameter "date"
  pure
    I.NewsFilter {nfDateRange = (\day -> I.NewsSinceUntil day day) <$> optDay}
