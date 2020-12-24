{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}

module Web.Handler.PatchTag
  ( run
  , Handle(..)
  ) where

import Control.Monad.Catch
import Core.Authentication
import qualified Core.Interactor.UpdateTag as IUpdateTag
import Core.Tag
import qualified Data.Aeson as A
import qualified Data.Aeson.TH as A
import Data.List
import Data.Maybe
import qualified Data.Text as T
import Web.Application
import qualified Web.Credentials
import Web.Exception

data Handle m =
  Handle
    { hLoadJSONRequestBody :: forall a. A.FromJSON a =>
                                          Request -> m a
    , hUpdateTag :: AuthenticatedUser -> TagId -> T.Text -> m (Either IUpdateTag.Failure Tag)
    , hAuthenticate :: Maybe Credentials -> m AuthenticatedUser
    , hPresent :: Tag -> Response
    }

run :: MonadThrow m => Handle m -> TagId -> GenericApplication m
run Handle {..} tagId request respond = do
  authUser <-
    hAuthenticate =<< Web.Credentials.getCredentialsFromRequest request
  InTag {inName} <- hLoadJSONRequestBody request
  hUpdateTag authUser tagId inName >>= \case
    Right tag -> respond $ hPresent tag
    Left IUpdateTag.UnknownTagId -> throwM ResourceNotFoundException
    Left IUpdateTag.TagNameMustNotBeEmpty ->
      throwM $ IncorrectParameterException "The tag name must not be empty"
    Left IUpdateTag.TagNameMustBeUnique ->
      throwM $
      IncorrectParameterException
        "The new tag name must be unique among all tags"

newtype InTag =
  InTag
    { inName :: T.Text
    }

$(A.deriveFromJSON
    A.defaultOptions
      {A.fieldLabelModifier = A.camelTo2 '_' . fromJust . stripPrefix "in"}
    ''InTag)
