{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ApplicativeDo #-}

module Web.Handler.GetNews
  ( Handle(..)
  , run
  ) where

import Control.Exception
import qualified Core.Interactor.GetNews as I
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
import qualified Web.QueryParameter as QP

data Handle =
  Handle
    { hGetNewsHandle :: I.Handle IO
    , hJSONEncode :: forall a. A.ToJSON a =>
                                 a -> BB.Builder
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
      pageQuery <-
        either (throwIO . queryParameterException) pure $
        QP.parseQuery (Wai.queryString request) pageQueryParser
      response <-
        catch
          (I.getNews (hGetNewsHandle h) pageQuery)
          (throwIO . BadRequestException . I.queryExceptionReason)
      pure $ presentResponse h response

pageQueryParser :: QP.QueryParser PageQuery
pageQueryParser = do
  pageQueryLimit <- QP.lookupP "limit"
  pageQueryOffset <- QP.lookupP "offset"
  pure PageQuery {..}

presentResponse :: Handle -> [I.News] -> BB.Builder
presentResponse Handle {..} = hJSONEncode . map presentNews
  where
    presentNews I.News {..} = News {..}

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
