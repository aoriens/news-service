{-# LANGUAGE RankNTypes #-}

module Web.Handler.PatchDraft
  ( run
  , Handle(..)
  ) where

import Control.Monad.Catch
import Core.Authentication
import Core.Category
import Core.EntityId
import Core.Image
import qualified Core.Interactor.UpdateDraft as I
import Core.News
import Core.Tag
import qualified Data.Aeson as A
import Data.Aeson ((.:!))
import qualified Data.HashSet as Set
import Data.Int
import qualified Data.Text as T
import Web.AppURI
import Web.Application
import Web.Credentials hiding (Credentials)
import Web.Exception
import Web.Representation.Image

data Handle m =
  Handle
    { hUpdateDraft :: AuthenticatedUser -> DraftId -> I.UpdateDraftRequest -> m (Either I.Failure Draft)
    , hLoadJSONRequestBody :: forall a. A.FromJSON a =>
                                          Request -> m a
    , hPresent :: Draft -> Response
    , hParseAppURI :: T.Text -> Maybe AppURI
    , hAuthenticate :: Maybe Credentials -> m AuthenticatedUser
    }

run :: MonadThrow m => Handle m -> DraftId -> GenericApplication m
run h@Handle {..} draftId request respond = do
  authUser <- hAuthenticate =<< getCredentialsFromRequest request
  body <- hLoadJSONRequestBody request
  updateDraftRequest <- makeUpdateDraftRequest h body
  hUpdateDraft authUser draftId updateDraftRequest >>= \case
    Right draft -> respond $ hPresent draft
    Left (I.UnknownEntityId [DraftEntityId _]) ->
      throwM ResourceNotFoundException
    Left (I.UnknownEntityId ids) ->
      throwM $ RelatedEntitiesNotFoundException ids

data RequestBody =
  RequestBody
    { rbTitle :: Maybe T.Text
    , rbText :: Maybe T.Text
    , rbCategoryId :: Maybe (Maybe Int32)
    , rbPhoto :: Maybe (Maybe ExistingOrNewImageRep)
    , rbPhotos :: Maybe [ExistingOrNewImageRep]
    , rbTagIds :: Maybe [Int32]
    }

instance A.FromJSON RequestBody where
  parseJSON =
    A.withObject "UpdateDraftRequest body" $ \o -> do
      rbTitle <- o .:! "title"
      rbText <- o .:! "text"
      rbCategoryId <- o .:! "category_id"
      rbPhoto <- o .:! "photo"
      rbPhotos <- o .:! "photos"
      rbTagIds <- o .:! "tag_ids"
      pure RequestBody {..}

makeUpdateDraftRequest ::
     MonadThrow m => Handle m -> RequestBody -> m I.UpdateDraftRequest
makeUpdateDraftRequest h RequestBody {..} = do
  udrMainPhoto <- mapM (mapM (parseImage h)) rbPhoto
  udrAdditionalPhotos <- mapM (mapM (parseImage h)) rbPhotos
  pure
    I.UpdateDraftRequest
      { udrTitle = rbTitle
      , udrText = rbText
      , udrCategory = fmap CategoryId <$> rbCategoryId
      , udrMainPhoto
      , udrAdditionalPhotos
      , udrTags = Set.fromList . map TagId <$> rbTagIds
      }

parseImage ::
     MonadThrow m
  => Handle m
  -> ExistingOrNewImageRep
  -> m (Either ImageId Image)
parseImage h = parseExistingOrNewImage $ hParseAppURI h
