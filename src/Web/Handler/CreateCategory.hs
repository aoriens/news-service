{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}

module Web.Handler.CreateCategory
  ( run
  , Handle(..)
  ) where

import Control.Monad.Catch
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
import Web.Credentials hiding (Credentials)
import Web.Exception

data Handle m =
  Handle
    { hCreateCategory :: AuthenticatedUser -> Maybe CategoryId -> NonEmpty T.Text -> m (Either ICreateCategory.Failure Category)
    , hLoadJSONRequestBody :: forall a. A.FromJSON a =>
                                          Request -> m a
    , hPresent :: Category -> Response
    , hAuthenticate :: Maybe Credentials -> m AuthenticatedUser
    }

run :: MonadThrow m => Handle m -> GenericApplication m
run Handle {..} request respond = do
  authUser <- hAuthenticate =<< getCredentialsFromRequest request
  InCategory {inNames, inParentId} <- hLoadJSONRequestBody request
  names <- getNames inNames
  hCreateCategory authUser (CategoryId <$> inParentId) names >>=
    either (throwM . exceptionFromFailure) (respond . hPresent)

getNames :: MonadThrow m => [T.Text] -> m (NonEmpty T.Text)
getNames = fromMaybeM (throwM exception) . nonEmpty
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
