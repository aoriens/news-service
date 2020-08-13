{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}

module Web.Handler.News
  ( Handle(..)
  , run
  ) where

import qualified Data.Aeson as A
import qualified Data.Aeson.TH as A
import Data.List
import Data.Maybe
import Data.Text (Text)
import Data.Time.Calendar
import qualified Interactor.GetNews as GetNews
import qualified Network.HTTP.Types as Http
import qualified Network.Wai as Wai
import qualified Network.Wai.Util as Wai
import Data.Int

newtype Handle =
  Handle
    { hGetNewsHandle :: GetNews.Handle IO
    }

run :: Handle -> Wai.Application
run h _ respond =
  respond $
  Wai.simpleResponseStream
    Http.ok200
    [(Http.hContentType, "application/json")]
    (A.fromEncoding . A.toEncoding . map presentNews <$>
     GetNews.getNews (hGetNewsHandle h))

presentNews :: GetNews.News -> News
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

