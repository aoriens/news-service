-- | Basic response constructors according to RFC2616. The file should
-- not contain application-specific logic for making requests, e.g.
-- specific content in bodies or non-standard headers.
module Web.Response
  ( ContentType
  , contentType
  , ResourceRepresentation(..)
  , dataResponse
  , noContentResponse
  , resourceCreatedAndReturnedResponse
  , resourceModifiedAndReturnedResponse
  , anotherResourceReturnedResponse
  ) where

import qualified Data.ByteString as B
import qualified Data.ByteString.Builder as BB
import qualified Data.Text.Encoding as T
import qualified Network.HTTP.Types as Http
import Web.AppURI
import Web.Application

-- | A value of @Content-Type@ header. It may be extended with content
-- type parameters in future.
newtype ContentType =
  ContentType
    { contentTypeName :: B.ByteString
    }

contentType :: B.ByteString -> ContentType
contentType = ContentType

data ResourceRepresentation =
  ResourceRepresentation
    { resourceRepresentationBody :: BB.Builder
    , resourceRepresentationContentType :: ContentType
    }

-- | A generic response containing data - 200 OK.
dataResponse :: ResourceRepresentation -> Response
dataResponse = makeResponse Http.ok200 []

-- | A response indicating no data in the body.
noContentResponse :: Response
noContentResponse = responseBuilder Http.noContent204 [] mempty

-- | Create a response indicating that a resource has just been
-- created and returned in the response body (e.g. POST).
resourceCreatedAndReturnedResponse ::
     AppURIConfig -> AppURI -> ResourceRepresentation -> Response
resourceCreatedAndReturnedResponse uriConfig appURI =
  makeResponse
    Http.created201
    [ locationHeaderWith uriConfig appURI
    , contentLocationHeaderWith uriConfig appURI
    ]

-- | Create a response containing a representation of a different
-- resource, with the resource URI.
anotherResourceReturnedResponse ::
     AppURIConfig -> AppURI -> ResourceRepresentation -> Response
anotherResourceReturnedResponse uriConfig appURI =
  makeResponse Http.ok200 [contentLocationHeaderWith uriConfig appURI]

-- | Create a response indicating that a resource has just been
-- updated and returned in the response body (PUT, PATCH, or sometimes
-- POST).
resourceModifiedAndReturnedResponse ::
     AppURIConfig -> AppURI -> ResourceRepresentation -> Response
resourceModifiedAndReturnedResponse uriConfig appURI =
  makeResponse Http.ok200 [contentLocationHeaderWith uriConfig appURI]

makeResponse ::
     Http.Status -> [Http.Header] -> ResourceRepresentation -> Response
makeResponse status headers ResourceRepresentation {..} =
  responseBuilder
    status
    (contentTypeHeaderWith resourceRepresentationContentType : headers)
    resourceRepresentationBody

contentTypeHeaderWith :: ContentType -> Http.Header
contentTypeHeaderWith ct = (Http.hContentType, contentTypeName ct)

contentLocationHeaderWith :: AppURIConfig -> AppURI -> Http.Header
contentLocationHeaderWith config uri =
  ("Content-Location", presentAppURI config uri)

locationHeaderWith :: AppURIConfig -> AppURI -> Http.Header
locationHeaderWith config uri = (Http.hLocation, presentAppURI config uri)

presentAppURI :: AppURIConfig -> AppURI -> B.ByteString
presentAppURI config = T.encodeUtf8 . renderAppURI config
