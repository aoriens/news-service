{-# LANGUAGE QuasiQuotes #-}

module Database.Logic.News.Exists
  ( newsExists
  ) where

import Core.News
import Data.Maybe
import Data.Profunctor
import Database.Service.Primitives
import qualified Hasql.TH as TH

newsExists :: NewsId -> Transaction Bool
newsExists =
  runStatement $
  dimap
    getNewsId
    isJust
    [TH.maybeStatement|
    select 1 :: integer
    from news
    where news_id = $1 :: integer
  |]
