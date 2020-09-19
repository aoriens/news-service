module Web.Response
  ( ContentType
  , contentType
  , ResourceRepresentation(..)
  , dataResponse
  , noContentResponse
  , resourceCreatedAndReturnedResponse
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

dataResponse :: ResourceRepresentation -> Wai.Response
dataResponse ResourceRepresentation {..} =
  Wai.responseBuilder
    Http.ok200
    [contentTypeHeaderWith resourceRepresentationContentType]
    resourceRepresentationBody

noContentResponse :: Wai.Response
noContentResponse = Wai.responseLBS Http.noContent204 [] mempty

resourceCreatedAndReturnedResponse ::
     AppURIConfig -> AppURI -> ResourceRepresentation -> Wai.Response
resourceCreatedAndReturnedResponse uriConfig appURI ResourceRepresentation {..} =
  Wai.responseBuilder
    Http.created201
    [ contentTypeHeaderWith resourceRepresentationContentType
    , locationHeaderWith uriConfig appURI
    , contentLocationHeaderWith uriConfig appURI
    ]
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
