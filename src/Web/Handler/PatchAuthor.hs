{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}

module Web.Handler.PatchAuthor
  ( run
  , Handle(..)
  ) where

import Control.Monad.Catch
import Core.Authentication
import Core.Author
import qualified Data.Aeson as A
import qualified Data.Aeson.TH as A
import Data.List
import Data.Maybe
import qualified Data.Text as T
import Web.Application
import Web.Credentials hiding (Credentials)

data Handle m =
  Handle
    { hUpdateAuthor :: AuthenticatedUser -> AuthorId -> T.Text -> m Author
    , hLoadJSONRequestBody :: forall a. A.FromJSON a =>
                                          Request -> m a
    , hPresent :: Author -> Response
    , hAuthenticate :: Maybe Credentials -> m AuthenticatedUser
    }

run :: MonadThrow m => Handle m -> AuthorId -> GenericApplication m
run Handle {..} authorId' request respond = do
  authUser <- hAuthenticate =<< getCredentialsFromRequest request
  InAuthor {inDescription} <- hLoadJSONRequestBody request
  newAuthor <- hUpdateAuthor authUser authorId' inDescription
  respond $ hPresent newAuthor

newtype InAuthor =
  InAuthor
    { inDescription :: T.Text
    }

$(A.deriveFromJSON
    A.defaultOptions
      {A.fieldLabelModifier = A.camelTo2 '_' . fromJust . stripPrefix "in"}
    ''InAuthor)
