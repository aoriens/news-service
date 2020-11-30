{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}

module Web.Handler.CreateCategory
  ( run
  , Handle(..)
  ) where

import Control.Exception
import Core.Authentication
import Core.Category
import qualified Core.Interactor.CreateCategory as ICreateCategory
import qualified Data.Aeson as A
import qualified Data.Aeson.TH as A
import Data.Int
import Data.List
import Data.List.NonEmpty
import Data.Maybe
import qualified Data.Text as T
import Web.Application
import Web.Credentials
import Web.Exception

data Handle =
  Handle
    { hCreateCategoryHandle :: ICreateCategory.Handle IO
    , hLoadJSONRequestBody :: forall a. A.FromJSON a =>
                                          Request -> IO a
    , hPresent :: Category -> Response
    , hAuthenticationHandle :: AuthenticationHandle IO
    }

run :: Handle -> Application
run Handle {..} request respond = do
  authUser <-
    authenticate hAuthenticationHandle =<< getCredentialsFromRequest request
  InCategory {inNames, inParentCategoryItemId} <- hLoadJSONRequestBody request
  names <-
    maybe
      (throwIO $ IncorrectParameterException "'names' array must not be empty")
      pure $
    nonEmpty inNames
  r <-
    ICreateCategory.run
      hCreateCategoryHandle
      authUser
      (CategoryId <$> inParentCategoryItemId)
      names
  case r of
    Right category -> respond $ hPresent category
    Left ICreateCategory.UnknownParentCategoryId ->
      throwIO $ IncorrectParameterException "Unknown parent category identifier"
    Left (ICreateCategory.IncorrectParameter reason) ->
      throwIO $ IncorrectParameterException reason

data InCategory =
  InCategory
    { inNames :: [T.Text]
    , inParentCategoryItemId :: Maybe Int32
    }

$(A.deriveFromJSON
    A.defaultOptions
      {A.fieldLabelModifier = A.camelTo2 '_' . fromJust . stripPrefix "in"}
    ''InCategory)
