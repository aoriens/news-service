{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}

module Web.Handler.CreateTag
  ( run
  , Handle(..)
  ) where

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
    , hPresenter :: ICreateTag.Result -> Response
    }

run :: Handle -> Application
run Handle {..} request respond = do
  creds <- getCredentialsFromRequest request
  InTag {inName} <- hLoadJSONRequestBody request
  result <- ICreateTag.run hCreateTagHandle creds inName
  respond $ hPresenter result

newtype InTag =
  InTag
    { inName :: T.Text
    }

$(A.deriveFromJSON
    A.defaultOptions
      {A.fieldLabelModifier = A.camelTo2 '_' . fromJust . stripPrefix "in"}
    ''InTag)
