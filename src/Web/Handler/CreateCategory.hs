{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}

module Web.Handler.CreateCategory
  ( run
  , Handle(..)
  ) where

import Control.Exception
import Core.Category
import qualified Core.Interactor.CreateCategory as ICreateCategory
import qualified Data.Aeson as A
import qualified Data.Aeson.TH as A
import Data.Int
import Data.List
import Data.List.NonEmpty
import Data.Maybe
import qualified Data.Text as T
import qualified Network.Wai as Wai
import Web.Credentials
import Web.Exception

data Handle =
  Handle
    { hCreateCategoryHandle :: ICreateCategory.Handle IO
    , hLoadJSONRequestBody :: forall a. A.FromJSON a =>
                                          Wai.Request -> IO a
    , hPresenter :: Category -> Wai.Response
    }

run :: Handle -> Wai.Application
run Handle {..} request respond = do
  credentials <- getCredentialsFromRequest request
  InCategory {inNames, inParentCategoryItemId} <- hLoadJSONRequestBody request
  names <-
    maybe
      (throwIO $ IncorrectParameterException "'names' array must not be empty")
      pure $
    nonEmpty inNames
  r <-
    ICreateCategory.run
      hCreateCategoryHandle
      credentials
      (CategoryId <$> inParentCategoryItemId)
      names
  case r of
    Right category -> respond $ hPresenter category
    Left ICreateCategory.UnknownParentCategoryId ->
      throwIO $ IncorrectParameterException "Unknown parent category identifier"

data InCategory =
  InCategory
    { inNames :: [T.Text]
    , inParentCategoryItemId :: Maybe Int32
    }

$(A.deriveFromJSON
    A.defaultOptions
      {A.fieldLabelModifier = A.camelTo2 '_' . fromJust . stripPrefix "in"}
    ''InCategory)
