module Web.BasicAuth
  ( credentialsFromRequest
  , credentialsFromAuthorizationHeader
  ) where

import Control.Monad
import Data.Bifunctor
import Data.ByteString.Base64 as B64
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Util as B
import Data.Char
import Data.Either.Util
import qualified Data.Text as T
import qualified Network.HTTP.Types as Http
import Web.Application

-- | Returns Left in case of malformed credentials, Right Nothing if
-- no credentials found, or Right (Just _) if credentials are found.
credentialsFromRequest ::
     Request -> Either T.Text (Maybe (B.ByteString, B.ByteString))
credentialsFromRequest request = do
  case lookup Http.hAuthorization $ requestHeaders request of
    Nothing -> Right Nothing
    Just value -> second Just $ credentialsFromAuthorizationHeader value

credentialsFromAuthorizationHeader ::
     B.ByteString -> Either T.Text (B.ByteString, B.ByteString)
credentialsFromAuthorizationHeader =
  stripAuthPrefix >=>
  first ("in base64 fragment: " <>) . B64.decodeBase64 >=> splitOnColon
  where
    stripAuthPrefix s =
      let (authType, creds) = B.break isSpace $ B.trimLeft s
       in if B.map toLower authType == "basic"
            then Right $ B.trim creds
            else Left "expected basic authorization"
    splitOnColon s =
      maybeToEither "colon is missing in credentials" $
      B.splitOnCharOnce (== ':') s
