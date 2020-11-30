{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}

module Web.Handler.CreateTag
  ( run
  , Handle(..)
  ) where

import Core.Authentication
import qualified Core.Interactor.CreateTag as ICreateTag
import qualified Data.Aeson as A
import qualified Data.Aeson.TH as A
import Data.List
import Data.Maybe
import qualified Data.Text as T
import Web.Application
import Web.Credentials

data Handle =
  Handle
    { hCreateTagHandle :: ICreateTag.Handle IO
    , hLoadJSONRequestBody :: forall a. A.FromJSON a =>
                                          Request -> IO a
    , hPresent :: ICreateTag.Result -> Response
    , hAuthenticationHandle :: AuthenticationHandle IO
    }

run :: Handle -> Application
run Handle {..} request respond = do
  authUser <-
    authenticate hAuthenticationHandle =<< getCredentialsFromRequest request
  InTag {inName} <- hLoadJSONRequestBody request
  result <- ICreateTag.run hCreateTagHandle authUser inName
  respond $ hPresent result

newtype InTag =
  InTag
    { inName :: T.Text
    }

$(A.deriveFromJSON
    A.defaultOptions
      {A.fieldLabelModifier = A.camelTo2 '_' . fromJust . stripPrefix "in"}
    ''InTag)
