{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}

module Web.RepresentationBuilder
  ( RepBuilderHandle(..)
  , RepBuilder
  , AppURL
  , renderAppURL
  , renderMaybeAppURL
  , runRepBuilder
  ) where

import Control.Monad.Reader
import qualified Data.Aeson as A
import qualified Data.ByteString.Builder as BB
import Data.Text as T
import qualified Network.HTTP.Types as Http
import qualified Network.Wai as Wai
import Web.AppURL
import Web.HTTP

data RepBuilderHandle =
  RepBuilderHandle
    { hJSONEncode :: forall a. A.ToJSON a =>
                                 a -> BB.Builder
    , hRenderAppURL :: AppURL -> Text
    }

newtype RepBuilder a =
  RepBuilder (Reader (AppURL -> T.Text) a)
  deriving (Functor, Applicative, Monad)

renderAppURL :: AppURL -> RepBuilder T.Text
renderAppURL url = RepBuilder $ asks ($ url)

renderMaybeAppURL :: Maybe AppURL -> RepBuilder (Maybe T.Text)
renderMaybeAppURL Nothing = pure Nothing
renderMaybeAppURL (Just u) = Just <$> renderAppURL u

runRepBuilder :: A.ToJSON a => RepBuilderHandle -> RepBuilder a -> Wai.Response
runRepBuilder h (RepBuilder r) =
  Wai.responseBuilder Http.ok200 [hJSONContentType] . hJSONEncode h $
  runReader r (hRenderAppURL h)
