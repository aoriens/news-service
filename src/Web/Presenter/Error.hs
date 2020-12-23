{-# LANGUAGE TemplateHaskell #-}

module Web.Presenter.Error
  ( presentWebException
  , presentCoreException
  , uncaughtExceptionResponseForDebug
  , methodNotAllowedResponse
  , notFoundResponse
  ) where

import Control.Exception
import Core.Exception
import Core.Permission
import qualified Data.Aeson as A
import qualified Data.Aeson.TH as A
import qualified Data.ByteString as B
import qualified Data.ByteString.Builder as BB
import Data.List
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Text.Show
import Network.HTTP.Types as Http
import Web.Application
import Web.Exception
import Web.RepresentationBuilder
import Web.Response

presentWebException :: RepBuilderHandle -> WebException -> Response
presentWebException h e =
  runResponseBuilder h $
  case e of
    BadRequestException reason -> badRequestBuilder reason
    IncorrectParameterException reason -> badRequestBuilder reason
    RelatedEntitiesNotFoundException ids ->
      badRequestBuilder $
      "The following entity IDs cannot be found: " <>
      (T.intercalate ", " . map showAsText) ids
    ResourceNotFoundException -> notFoundBuilder
    UnsupportedMediaTypeException supportedTypes ->
      errorEntityBuilder Http.unsupportedMediaType415 [] $
      "Encountered an unsupported media type. Supported media types are: " <>
      T.intercalate ", " supportedTypes
    PayloadTooLargeException maxPayloadSize ->
      errorEntityBuilder Http.requestEntityTooLarge413 [] $
      "The request body length must not exceed " <>
      showAsText maxPayloadSize <> " bytes"
    MalformedAuthDataException _ -> notFoundBuilder

presentCoreException :: RepBuilderHandle -> CoreException -> Response
presentCoreException h e =
  runResponseBuilder h $
  case e of
    QueryException reason -> badRequestBuilder reason
    BadCredentialsException _ -> unauthorizedBuilder "Incorrect credentials"
    AuthenticationRequired -> authenticationRequired
    NoPermissionException perm _
      | AdminPermission <- perm -> notFoundBuilder
      | AuthorshipPermission _ <- perm ->
        unauthorizedBuilder
          "Operation is only allowed to a specific author that you do not own. Forgot to authorize?"
      | AdminOrSpecificUserPermission _ <- perm -> authenticationRequired
    RequestedEntityNotFoundException _ -> notFoundBuilder
    DependentEntitiesNotFoundException ids ->
      badRequestBuilder $
      "The following entity IDs cannot be found: " <>
      (T.intercalate ", " . map showAsText) ids
    DisallowedImageContentTypeException badContentType allowedContentTypes ->
      errorEntityBuilder
        Http.unsupportedMediaType415
        []
        ("Unsupported image content type: " <>
         badContentType <>
         ". Supported content types: " <> T.intercalate ", " allowedContentTypes)

badRequestBuilder :: T.Text -> ResponseBuilder
badRequestBuilder = errorEntityBuilder Http.badRequest400 []

notFoundBuilder :: ResponseBuilder
notFoundBuilder = const notFoundResponse

notFoundResponse :: Response
notFoundResponse = htmlErrorResponse Http.notFound404 [] ""

authenticationRequired :: ResponseBuilder
authenticationRequired = unauthorizedBuilder "Authentication required"

unauthorizedBuilder :: T.Text -> ResponseBuilder
unauthorizedBuilder =
  errorEntityBuilder
    Http.unauthorized401
    [("WWW-Authenticate", "Basic realm=\"\"")]

uncaughtExceptionResponseForDebug :: SomeException -> Response
uncaughtExceptionResponseForDebug e =
  htmlErrorResponse Http.internalServerError500 [] $
  "<pre>" <> showAsText e <> "</pre>"

methodNotAllowedResponse :: [B.ByteString] -> Response
methodNotAllowedResponse knownMethods =
  htmlErrorResponse Http.methodNotAllowed405 [makeAllowHeader knownMethods] ""
  where
    makeAllowHeader methods = ("Allow", B.intercalate ", " methods)

-- An intermediate type for making a request, free from external
-- dependencies
type ResponseBuilder = RepBuilderHandle -> Response

runResponseBuilder :: RepBuilderHandle -> ResponseBuilder -> Response
runResponseBuilder = flip ($)

htmlErrorResponse :: Http.Status -> [Http.Header] -> T.Text -> Response
htmlErrorResponse status additionalHeaders reason =
  responseWithBuilder
    status
    ((Http.hContentType, "text/html") : additionalHeaders)
    body
  where
    body =
      mconcat
        [ "<!DOCTYPE html><html><body><h1>"
        , BB.stringUtf8 (show (Http.statusCode status))
        , " "
        , BB.byteString (Http.statusMessage status)
        , "</h1><p>"
        , BB.byteString $ T.encodeUtf8 reason
        , "</p></body></html>\n"
        ]

errorEntityBuilder :: Http.Status -> [Http.Header] -> T.Text -> ResponseBuilder
errorEntityBuilder status headers reason repBuilderHandle =
  representationResponse status headers $
  runRepBuilder repBuilderHandle $ errorRepWithReason reason

errorRepWithReason :: T.Text -> RepBuilder ErrorRep
errorRepWithReason errReason = pure ErrorRep {errReason}

newtype ErrorRep =
  ErrorRep
    { errReason :: T.Text
    }

$(A.deriveToJSON
    A.defaultOptions
      { A.fieldLabelModifier = A.camelTo2 '_' . fromJust . stripPrefix "err"
      , A.omitNothingFields = True
      }
    ''ErrorRep)
