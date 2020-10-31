{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}

module Web.RepresentationBuilder
  ( RepBuilderHandle(..)
  , RepBuilder
  , AppURI
  , renderAppURI
  , runRepBuilder
  , AppURIRep
  ) where

import Control.Monad.Reader
import qualified Data.Aeson as A
import qualified Data.ByteString.Builder as BB
import Data.Text as T
import Web.AppURI hiding (renderAppURI)
import Web.Representation.AppURI
import Web.Response

data RepBuilderHandle =
  RepBuilderHandle
    { hJSONEncode :: forall a. A.ToJSON a =>
                                 a -> BB.Builder
    , hRenderAppURI :: AppURI -> Text
    }

-- | A monad type which is parameterized with the representation type
--  it can generate.
newtype RepBuilder a =
  RepBuilder (Reader (AppURI -> AppURIRep) a)
  deriving (Functor, Applicative, Monad)

renderAppURI :: AppURI -> RepBuilder AppURIRep
renderAppURI url = RepBuilder $ asks ($ url)

runRepBuilder ::
     A.ToJSON a => RepBuilderHandle -> RepBuilder a -> ResourceRepresentation
runRepBuilder h (RepBuilder r) =
  ResourceRepresentation
    { resourceRepresentationBody =
        hJSONEncode h $ runReader r (AppURIRep . hRenderAppURI h)
    , resourceRepresentationContentType = contentType "application/json"
    }
