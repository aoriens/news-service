{-# LANGUAGE RankNTypes #-}

module Web.Handler.PatchCategory
  ( run
  , Handle(..)
  ) where

import Control.Monad.Catch
import Core.Authentication
import Core.Category
import qualified Core.Interactor.UpdateCategory as IUpdateCategory
import qualified Data.Aeson as A
import Data.Aeson ((.:!))
import qualified Data.Text as T
import Web.Application
import Web.Credentials hiding (Credentials)
import Web.Exception

data Handle m =
  Handle
    { hUpdateCategory :: AuthenticatedUser -> IUpdateCategory.Request -> m (Either IUpdateCategory.Failure Category)
    , hLoadJSONRequestBody :: forall a. A.FromJSON a =>
                                          Request -> m a
    , hPresent :: Category -> Response
    , hAuthenticate :: Maybe Credentials -> m AuthenticatedUser
    }

run :: MonadThrow m => Handle m -> CategoryId -> GenericApplication m
run Handle {..} catId request respond = do
  authUser <- hAuthenticate =<< getCredentialsFromRequest request
  body <- hLoadJSONRequestBody request
  hUpdateCategory authUser (updateCategoryRequestFromBody catId body) >>= \case
    Right cat -> respond $ hPresent cat
    Left failure -> throwM $ exceptionFromFailure failure

updateCategoryRequestFromBody ::
     CategoryId -> RequestBody -> IUpdateCategory.Request
updateCategoryRequestFromBody catId RequestBody {..} =
  IUpdateCategory.Request {rCategoryId = catId, rNewName, rNewParent}

exceptionFromFailure :: IUpdateCategory.Failure -> WebException
exceptionFromFailure =
  \case
    IUpdateCategory.UnknownCategoryId -> ResourceNotFoundException
    IUpdateCategory.UnknownNewParentId ->
      IncorrectParameterException "Unknown new parent category ID"
    IUpdateCategory.NameMustNotBeEmpty ->
      IncorrectParameterException "The new category name must not be empty"
    IUpdateCategory.NameMustBeUniqueAmongSiblings ->
      IncorrectParameterException
        "The new category name must be unique among its sibling categories"
    IUpdateCategory.AncestryLoopDetected ->
      IncorrectParameterException
        "Changing the category parent must not create an ancestry loop"

data RequestBody =
  RequestBody
    { rNewName :: Maybe T.Text
    , rNewParent :: Maybe (Maybe CategoryId)
    }

instance A.FromJSON RequestBody where
  parseJSON =
    A.withObject "Request" $ \o -> do
      rNewName <- o .:! "name"
      rNewParent <- fmap (fmap CategoryId) <$> o .:! "parent_id"
      pure RequestBody {rNewName, rNewParent}
