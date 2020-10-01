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
import qualified Web.QueryParameter as QP
import qualified Web.QueryParameter.PageQuery as QP

data Handle =
  Handle
    { hGetNewsHandle :: I.Handle IO
    , hJSONEncode :: forall a. A.ToJSON a =>
                                 a -> BB.Builder
    , hPresenter :: [News] -> Response
    }

run :: Handle -> Application
run Handle {..} request respond = do
  pageQuery <- QP.parseQueryM (requestQueryString request) QP.parsePageQuery
  news <- I.getNews hGetNewsHandle pageQuery
  respond $ hPresenter news
