{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ApplicativeDo #-}

module Web.QueryParameter.PageQuery
  ( parsePageQuery
  ) where

import Core.Pagination
import qualified Web.QueryParameter as QP

parsePageQuery :: QP.QueryParser PageQuery
parsePageQuery = do
  pageQueryLimit <- QP.lookup "limit"
  pageQueryOffset <- QP.lookup "offset"
  pure PageQuery {..}
