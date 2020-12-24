{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}

module Web.Handler.CreateTag
  ( run
  , Handle(..)
  ) where

import Control.Monad.Catch
import Core.Authentication
import qualified Core.Interactor.CreateTag as ICreateTag
import qualified Data.Aeson as A
import qualified Data.Aeson.TH as A
import Data.List
import Data.Maybe
import qualified Data.Text as T
import Web.Application
import Web.Credentials hiding (Credentials)

data Handle m =
  Handle
    { hCreateTag :: AuthenticatedUser -> T.Text -> m ICreateTag.Result
    , hLoadJSONRequestBody :: forall a. A.FromJSON a =>
                                          Request -> m a
    , hPresent :: ICreateTag.Result -> Response
    , hAuthenticate :: Maybe Credentials -> m AuthenticatedUser
    }

run :: MonadThrow m => Handle m -> GenericApplication m
run Handle {..} request respond = do
  authUser <- hAuthenticate =<< getCredentialsFromRequest request
  InTag {inName} <- hLoadJSONRequestBody request
  result <- hCreateTag authUser inName
  respond $ hPresent result

newtype InTag =
  InTag
    { inName :: T.Text
    }

$(A.deriveFromJSON
    A.defaultOptions
      {A.fieldLabelModifier = A.camelTo2 '_' . fromJust . stripPrefix "in"}
    ''InTag)
