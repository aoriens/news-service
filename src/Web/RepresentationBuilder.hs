{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}

module Web.RepresentationBuilder
  ( Handle(..)
  , Builder
  , AppURL
  , renderAppURL
  , renderMaybeAppURL
  , runBuilder
  ) where

import Control.Monad.Reader
import qualified Data.Aeson as A
import qualified Data.ByteString.Builder as BB
import Data.Text as T
import Web.AppURL

data Handle =
  Handle
    { hJSONEncode :: forall a. A.ToJSON a =>
                                 a -> BB.Builder
    , hRenderAppURL :: AppURL -> Text
    }

newtype Builder a =
  Builder (Reader (AppURL -> T.Text) a)
  deriving (Functor, Applicative, Monad)

renderAppURL :: AppURL -> Builder T.Text
renderAppURL url = Builder $ asks ($ url)

renderMaybeAppURL :: Maybe AppURL -> Builder (Maybe T.Text)
renderMaybeAppURL Nothing = pure Nothing
renderMaybeAppURL (Just u) = Just <$> renderAppURL u

runBuilder :: A.ToJSON a => Handle -> Builder a -> BB.Builder
runBuilder h (Builder r) = hJSONEncode h $ runReader r (hRenderAppURL h)
