module Web.BasicAuth
  ( credentialsFromRequest
  , credentialsFromAuthorizationHeader
  ) where

import Control.Monad
import Data.Bifunctor
import Data.ByteString.Base64 as B64
import qualified Data.ByteString.Char8 as B
import Data.Char
import qualified Data.Text as T
import qualified Network.HTTP.Types as Http
import qualified Network.Wai as Wai

-- | Returns Left in case of malformed credentials, Right Nothing if
-- no credentials found, or Right (Just _) if credentials are found.
credentialsFromRequest ::
     Wai.Request -> Either T.Text (Maybe (B.ByteString, B.ByteString))
credentialsFromRequest request = do
  case lookup Http.hAuthorization $ Wai.requestHeaders request of
    Nothing -> Right Nothing
    Just value -> second Just $ credentialsFromAuthorizationHeader value

credentialsFromAuthorizationHeader ::
     B.ByteString -> Either T.Text (B.ByteString, B.ByteString)
credentialsFromAuthorizationHeader =
  stripAuthPrefix >=>
  first ("in base64 fragment: " <>) . B64.decodeBase64 >=> splitOnColon
  where
    stripAuthPrefix s =
      let (authType, creds) = B.break isSpace $ trimLeft s
       in if B.map toLower authType == "basic"
            then Right $ trim creds
            else Left "Expected basic authorization"
    trimLeft = B.dropWhile isSpace
    trim = trimRight . trimLeft
    trimRight = fst . B.spanEnd isSpace
    splitOnColon s =
      let (login, p) = B.break (== ':') s
       in case B.uncons p of
            Just (_, password) -> Right (login, password)
            Nothing -> Left "colon is missing in credentials"
