{-# LANGUAGE ApplicativeDo #-}

module Web.QueryParameter.PageQuery
  ( parsePageQuery
  ) where

import Core.Pagination
import Web.QueryParameter

parsePageQuery :: QueryParser PageSpecQuery
parsePageQuery = do
  pageQueryLimit <- lookupQueryParameter "limit"
  pageQueryOffset <- lookupQueryParameter "offset"
  pure PageSpecQuery {..}
