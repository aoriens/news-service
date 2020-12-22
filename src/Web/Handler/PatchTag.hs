{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}

module Web.Handler.PatchTag
  ( run
  , Handle(..)
  ) where

import Control.Exception
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

data Handle =
  Handle
    { hLoadJSONRequestBody :: forall a. A.FromJSON a =>
                                          Request -> IO a
    , hUpdateTag :: AuthenticatedUser -> TagId -> T.Text -> IO (Either IUpdateTag.Failure Tag)
    , hAuthenticate :: Maybe Credentials -> IO AuthenticatedUser
    , hPresent :: Tag -> Response
    }

run :: Handle -> TagId -> Application
run Handle {..} tagId request respond = do
  authUser <-
    hAuthenticate =<< Web.Credentials.getCredentialsFromRequest request
  InTag {inName} <- hLoadJSONRequestBody request
  hUpdateTag authUser tagId inName >>= \case
    Right tag -> respond $ hPresent tag
    Left IUpdateTag.UnknownTagId -> throwIO ResourceNotFoundException
    Left IUpdateTag.TagNameMustNotBeEmpty ->
      throwIO $ IncorrectParameterException "The tag name must not be empty"
    Left IUpdateTag.TagNameMustBeUnique ->
      throwIO $
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
