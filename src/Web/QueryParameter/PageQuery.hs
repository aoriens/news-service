{-# LANGUAGE ApplicativeDo #-}

module Web.QueryParameter.PageQuery
  ( parsePageQuery
  ) where

import Core.Pagination
import qualified Web.QueryParameter as QueryParameter

parsePageQuery :: QueryParameter.Parser PageSpecQuery
parsePageQuery = do
  pageQueryLimit <- QueryParameter.lookup "limit"
  pageQueryOffset <- QueryParameter.lookup "offset"
  pure PageSpecQuery {..}
