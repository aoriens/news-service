{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}

module Web.Handler.News
  ( Handle(..)
  , run
  ) where

import Control.Exception
import qualified Core.Interactor.GetNews as GetNews
import Core.Pagination
import qualified Data.Aeson as A
import qualified Data.Aeson.TH as A
import qualified Data.ByteString.Builder as BB
import Data.Int
import Data.List
import Data.Maybe
import Data.Text (Text)
import Data.Time.Calendar
import qualified Network.HTTP.Types as Http
import qualified Network.Wai as Wai
import qualified Network.Wai.Util as Wai
import Web.Exception
import Web.QueryParameter

newtype Handle =
  Handle
    { hGetNewsHandle :: GetNews.Handle IO
    }

run :: Handle -> Wai.Application
run h request respond =
  respond $
  Wai.simpleResponseStream
    Http.ok200
    [(Http.hContentType, "application/json")]
    getResponse
  where
    getResponse = do
      page <- either (throwIO . BadRequestException) pure $ parsePage request
      response <- GetNews.getNews (hGetNewsHandle h) page
      pure $ presentResponse response

parsePage :: Wai.Request -> Either Text PageQuery
parsePage request = do
  let query = Wai.queryString request
  pageQueryLimit <- fmap PageLimit <$> lookupQueryParameter "limit" query
  pageQueryOffset <- fmap PageOffset <$> lookupQueryParameter "offset" query
  pure PageQuery {..}

presentResponse :: [GetNews.News] -> BB.Builder
presentResponse = A.fromEncoding . A.toEncoding . map presentNews
  where
    presentNews GetNews.News {..} = News {..}

data News =
  News
    { newsId :: Int32
    , newsTitle :: Text
    , newsDate :: Day
    , newsText :: Text
    }

$(A.deriveToJSON
    A.defaultOptions
      {A.fieldLabelModifier = A.camelTo2 '_' . fromJust . stripPrefix "news"}
    ''News)
