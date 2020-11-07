{-# LANGUAGE RankNTypes #-}

module Web.Handler.GetNews
  ( Handle(..)
  , run
  ) where

import qualified Core.Interactor.GetNews as I
import Core.News
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
  pageQuery <- parseQueryM (requestQueryString request) parsePageQuery
  news <- I.getNews hGetNewsHandle I.emptyNewsFilter pageQuery
  respond $ hPresenter news
