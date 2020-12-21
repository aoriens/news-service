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
import Data.Maybe.Util
import qualified Data.Text as T
import Web.Application
import Web.Credentials
import Web.Exception

data Handle =
  Handle
    { hCreateCategory :: AuthenticatedUser -> Maybe CategoryId -> NonEmpty T.Text -> IO (Either ICreateCategory.Failure Category)
    , hLoadJSONRequestBody :: forall a. A.FromJSON a =>
                                          Request -> IO a
    , hPresent :: Category -> Response
    , hAuthenticationHandle :: AuthenticationHandle IO
    }

run :: Handle -> Application
run Handle {..} request respond = do
  authUser <-
    authenticate hAuthenticationHandle =<< getCredentialsFromRequest request
  InCategory {inNames, inParentId} <- hLoadJSONRequestBody request
  names <- getNames inNames
  hCreateCategory authUser (CategoryId <$> inParentId) names >>=
    either (throwIO . exceptionFromFailure) (respond . hPresent)

getNames :: [T.Text] -> IO (NonEmpty T.Text)
getNames = fromMaybeM (throwIO exception) . nonEmpty
  where
    exception = IncorrectParameterException "'names' array must not be empty"

exceptionFromFailure :: ICreateCategory.Failure -> WebException
exceptionFromFailure =
  IncorrectParameterException . \case
    ICreateCategory.UnknownParentCategoryId ->
      "Unknown parent category identifier"
    ICreateCategory.CategoryNameMustNotBeEmpty ->
      "Category name must not be empty"
    ICreateCategory.CategoryNameMustBeUniqueAmongSiblings ->
      "Category name must be unique among sibling categories"

data InCategory =
  InCategory
    { inNames :: [T.Text]
    , inParentId :: Maybe Int32
    }

$(A.deriveFromJSON
    A.defaultOptions
      {A.fieldLabelModifier = A.camelTo2 '_' . fromJust . stripPrefix "in"}
    ''InCategory)
