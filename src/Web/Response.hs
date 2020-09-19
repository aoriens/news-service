module Web.Response
  ( ContentType
  , contentType
  , ResourceRepresentation(..)
  , dataResponse
  , noContentResponse
  ) where

import qualified Data.ByteString as B
import qualified Data.ByteString.Builder as BB
import qualified Network.HTTP.Types as Http
import qualified Network.Wai as Wai

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

contentTypeHeaderWith :: ContentType -> Http.Header
contentTypeHeaderWith ct = (Http.hContentType, contentTypeName ct)
