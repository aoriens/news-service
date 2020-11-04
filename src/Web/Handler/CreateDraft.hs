{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}

module Web.Handler.CreateDraft
  ( run
  , Handle(..)
  ) where

import Control.Monad.Catch
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
import Web.Credentials
import Web.Exception
import Web.Representation.AppURI
import Web.Representation.Image

data Handle =
  Handle
    { hCreateDraftHandle :: I.Handle IO
    , hLoadJSONRequestBody :: forall a. A.FromJSON a =>
                                          Request -> IO a
    , hPresenter :: NewsVersion -> Response
    , hParseAppURI :: T.Text -> Maybe AppURI
    }

run :: Handle -> Application
run h@Handle {..} request respond = do
  creds <- getCredentialsFromRequest request
  inDraft <- hLoadJSONRequestBody request
  createDraftRequest <- makeCreateDraftRequest h inDraft
  version <- I.run hCreateDraftHandle creds createDraftRequest
  respond $ hPresenter version

data InDraft =
  InDraft
    { inTitle :: T.Text
    , inText :: T.Text
    , inAuthorId :: Int32
    , inCategoryId :: Int32
    , inPhoto :: Maybe ExistingOrNewImageRep
    , inPhotos :: Maybe [ExistingOrNewImageRep]
    , inTagIds :: Maybe [Int32]
    }

makeCreateDraftRequest ::
     MonadThrow m => Handle -> InDraft -> m I.CreateDraftRequest
makeCreateDraftRequest h InDraft {..} = do
  cdMainPhoto <- mapM (parseImage h) inPhoto
  cdAdditionalPhotos <- mapM (parseImage h) $ fromMaybe [] inPhotos
  pure
    I.CreateDraftRequest
      { cdTitle = inTitle
      , cdText = inText
      , cdAuthorId = Just $ AuthorId inAuthorId
      , cdCategoryId = CategoryId inCategoryId
      , cdMainPhoto
      , cdAdditionalPhotos
      , cdTagIds = maybe Set.empty (Set.fromList . map TagId) inTagIds
      }

parseImage ::
     MonadThrow m => Handle -> ExistingOrNewImageRep -> m (Either ImageId Image)
parseImage h rep =
  case rep of
    NewImage imageRep -> pure . Right $ imageFromRep imageRep
    ExistingImage (AppURIRep uriText) ->
      case hParseAppURI h uriText of
        Just (ImageURI imageIdent) -> pure $ Left imageIdent
        _ ->
          throwM . IncorrectParameterException $
          "URI does not match an image resource: " <> uriText

$(A.deriveFromJSON
    A.defaultOptions
      {A.fieldLabelModifier = A.camelTo2 '_' . fromJust . stripPrefix "in"}
    ''InDraft)
