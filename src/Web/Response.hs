module Web.Response
  ( ContentType
  , contentType
  , ResourceRepresentation(..)
  , dataResponse
  , noContentResponse
  , resourceCreatedAndReturnedResponse
  , resourceModifiedAndReturnedResponse
  ) where

import qualified Data.ByteString as B
import qualified Data.ByteString.Builder as BB
import qualified Data.Text.Encoding as T
import qualified Network.HTTP.Types as Http
import qualified Network.Wai as Wai
import Web.AppURI

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
dataResponse :: ResourceRepresentation -> Wai.Response
dataResponse = responseWithContent Http.ok200 []

-- | A response indicating no data in the body.
noContentResponse :: Wai.Response
noContentResponse = Wai.responseLBS Http.noContent204 [] mempty

-- | Create a response indicating that a resource has just been
-- created and returned in the response body (e.g. POST).
resourceCreatedAndReturnedResponse ::
     AppURIConfig -> AppURI -> ResourceRepresentation -> Wai.Response
resourceCreatedAndReturnedResponse uriConfig appURI =
  responseWithContent
    Http.created201
    [ locationHeaderWith uriConfig appURI
    , contentLocationHeaderWith uriConfig appURI
    ]

-- | Create a response indicating that a resource has just been
-- updated and returned in the response body (PUT, PATCH, or sometimes
-- POST).
resourceModifiedAndReturnedResponse ::
     AppURIConfig -> AppURI -> ResourceRepresentation -> Wai.Response
resourceModifiedAndReturnedResponse uriConfig appURI =
  responseWithContent Http.ok200 [contentLocationHeaderWith uriConfig appURI]

responseWithContent ::
     Http.Status -> [Http.Header] -> ResourceRepresentation -> Wai.Response
responseWithContent status headers ResourceRepresentation {..} =
  Wai.responseBuilder
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
