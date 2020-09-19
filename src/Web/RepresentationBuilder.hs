{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}

module Web.RepresentationBuilder
  ( RepBuilderHandle(..)
  , RepBuilder
  , AppURI
  , renderAppURI
  , renderMaybeAppURI
  , runRepBuilder
  ) where

import Control.Monad.Reader
import qualified Data.Aeson as A
import qualified Data.ByteString.Builder as BB
import Data.Text as T
import Web.AppURI hiding (renderAppURI)
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
  RepBuilder (Reader (AppURI -> T.Text) a)
  deriving (Functor, Applicative, Monad)

renderAppURI :: AppURI -> RepBuilder T.Text
renderAppURI url = RepBuilder $ asks ($ url)

renderMaybeAppURI :: Maybe AppURI -> RepBuilder (Maybe T.Text)
renderMaybeAppURI Nothing = pure Nothing
renderMaybeAppURI (Just u) = Just <$> renderAppURI u

runRepBuilder ::
     A.ToJSON a => RepBuilderHandle -> RepBuilder a -> ResourceRepresentation
runRepBuilder h (RepBuilder r) =
  ResourceRepresentation
    { resourceRepresentationBody = hJSONEncode h $ runReader r (hRenderAppURI h)
    , resourceRepresentationContentType = contentType "application/json"
    }
