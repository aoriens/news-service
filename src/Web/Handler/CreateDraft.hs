{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}

module Web.Handler.CreateDraft
  ( run
  , Handle(..)
  ) where

import Control.Monad.Catch
import Core.Authentication
import Core.Author
import Core.Category
import Core.Image
import qualified Core.Interactor.CreateDraft as I
import Core.News
import Core.Tag
import qualified Data.Aeson as A
import qualified Data.Aeson.TH as A
import qualified Data.HashSet as Set
import Data.Int
import Data.List
import Data.Maybe
import qualified Data.Text as T
import Web.AppURI
import Web.Application
import Web.Credentials hiding (Credentials)
import Web.Representation.Image

data Handle m =
  Handle
    { hCreateDraft :: AuthenticatedUser -> I.CreateDraftRequest -> m Draft
    , hLoadJSONRequestBody :: forall a. A.FromJSON a =>
                                          Request -> m a
    , hPresent :: Draft -> Response
    , hParseAppURI :: T.Text -> Maybe AppURI
    , hAuthenticate :: Maybe Credentials -> m AuthenticatedUser
    }

run :: MonadThrow m => Handle m -> GenericApplication m
run h@Handle {..} request respond = do
  authUser <- hAuthenticate =<< getCredentialsFromRequest request
  inDraft <- hLoadJSONRequestBody request
  createDraftRequest <- makeCreateDraftRequest h inDraft
  version <- hCreateDraft authUser createDraftRequest
  respond $ hPresent version

data InDraft =
  InDraft
    { inTitle :: T.Text
    , inText :: T.Text
    , inAuthorId :: Maybe Int32
    , inCategoryId :: Maybe Int32
    , inPhoto :: Maybe ExistingOrNewImageRep
    , inPhotos :: Maybe [ExistingOrNewImageRep]
    , inTagIds :: Maybe [Int32]
    }

makeCreateDraftRequest ::
     MonadThrow m => Handle m -> InDraft -> m I.CreateDraftRequest
makeCreateDraftRequest h InDraft {..} = do
  cdMainPhoto <- mapM (parseImage h) inPhoto
  cdAdditionalPhotos <- mapM (parseImage h) $ fromMaybe [] inPhotos
  pure
    I.CreateDraftRequest
      { cdTitle = inTitle
      , cdText = inText
      , cdAuthorId = AuthorId <$> inAuthorId
      , cdCategoryId = CategoryId <$> inCategoryId
      , cdMainPhoto
      , cdAdditionalPhotos
      , cdTagIds = maybe Set.empty (Set.fromList . map TagId) inTagIds
      }

parseImage ::
     MonadThrow m
  => Handle m
  -> ExistingOrNewImageRep
  -> m (Either ImageId Image)
parseImage h = parseExistingOrNewImage (hParseAppURI h)

$(A.deriveFromJSON
    A.defaultOptions
      {A.fieldLabelModifier = A.camelTo2 '_' . fromJust . stripPrefix "in"}
    ''InDraft)
