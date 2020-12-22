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
import qualified Data.ByteString as B
import qualified Data.ByteString.Builder as BB
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Text.Show
import Network.HTTP.Types as Http
import Web.Application
import Web.Exception

presentWebException :: WebException -> Response
presentWebException e =
  case e of
    BadRequestException reason -> badRequestResponse reason
    IncorrectParameterException reason -> badRequestResponse reason
    RelatedEntitiesNotFoundException ids ->
      stubErrorResponseWithReason Http.badRequest400 [] $
      "The following entity IDs cannot be found: " <>
      (T.intercalate ", " . map showAsText) ids
    ResourceNotFoundException -> notFoundResponse
    UnsupportedMediaTypeException supportedTypes ->
      stubErrorResponseWithReason Http.unsupportedMediaType415 [] $
      "Supported media types are: " <> T.intercalate ", " supportedTypes
    PayloadTooLargeException maxPayloadSize ->
      stubErrorResponseWithReason Http.requestEntityTooLarge413 [] $
      "The request body length must not exceed " <>
      showAsText maxPayloadSize <> " bytes"
    MalformedAuthDataException _ -> notFoundResponse

presentCoreException :: CoreException -> Response
presentCoreException e =
  case e of
    QueryException reason -> badRequestResponse reason
    BadCredentialsException _ -> notFoundResponse
    AuthenticationRequired -> unauthorizedResponse
    NoPermissionException perm _
      | AdminPermission <- perm -> notFoundResponse
      | AuthorshipPermission _ <- perm ->
        stubErrorResponseWithReason
          Http.forbidden403
          []
          "Operation is only allowed to a specific author that you do not own. Forgot to authorize?"
      | AdminOrSpecificUserPermission _ <- perm -> unauthorizedResponse
    UserNotIdentifiedException _ ->
      stubErrorResponseWithReason
        Http.forbidden403
        []
        "Authentication is required"
    RequestedEntityNotFoundException _ -> notFoundResponse
    DependentEntitiesNotFoundException ids ->
      stubErrorResponseWithReason Http.badRequest400 [] $
      "The following entity IDs cannot be found: " <>
      (T.intercalate ", " . map showAsText) ids
    DisallowedImageContentTypeException badContentType allowedContentTypes ->
      stubErrorResponseWithReason
        Http.unsupportedMediaType415
        []
        ("Unsupported image content type: " <>
         badContentType <>
         ". Supported content types: " <> T.intercalate ", " allowedContentTypes)

notFoundResponse :: Response
notFoundResponse = stubErrorResponse Http.notFound404 []

unauthorizedResponse :: Response
unauthorizedResponse =
  stubErrorResponse
    Http.unauthorized401
    [("WWW-Authenticate", "Basic realm=\"\"")]

badRequestResponse :: T.Text -> Response
badRequestResponse = stubErrorResponseWithReason Http.badRequest400 []

uncaughtExceptionResponseForDebug :: SomeException -> Response
uncaughtExceptionResponseForDebug e =
  stubErrorResponseWithReason Http.internalServerError500 [] $
  "<pre>" <> showAsText e <> "</pre>"

methodNotAllowedResponse :: [B.ByteString] -> Response
methodNotAllowedResponse knownMethods =
  stubErrorResponse Http.methodNotAllowed405 [makeAllowHeader knownMethods]
  where
    makeAllowHeader methods = ("Allow", B.intercalate ", " methods)

stubErrorResponse :: Http.Status -> [Http.Header] -> Response
stubErrorResponse status additionalHeaders =
  stubErrorResponseWithReason status additionalHeaders ""

stubErrorResponseWithReason ::
     Http.Status -> [Http.Header] -> T.Text -> Response
stubErrorResponseWithReason status additionalHeaders reason =
  responseBuilder
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
