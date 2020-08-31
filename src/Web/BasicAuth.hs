module Web.BasicAuth
  ( credentialsFromRequest
  , credentialsFromAuthorizationHeader
  ) where

import Control.Monad
import Data.ByteString.Base64 as B64
import qualified Data.ByteString.Char8 as B
import Data.Char
import qualified Network.HTTP.Types as Http
import qualified Network.Wai as Wai

-- | Returns Left in case of a malformed credentials, Right Nothing if
-- no credentials found, or Right (Just _) if credentials are found.
credentialsFromRequest ::
     Wai.Request -> Either () (Maybe (B.ByteString, B.ByteString))
credentialsFromRequest request = do
  case lookup Http.hAuthorization $ Wai.requestHeaders request of
    Nothing -> Right Nothing
    Just value ->
      maybe (Left ()) (Right . Just) $ credentialsFromAuthorizationHeader value

-- | Returns Nothing in case of a malformed value.
credentialsFromAuthorizationHeader ::
     B.ByteString -> Maybe (B.ByteString, B.ByteString)
credentialsFromAuthorizationHeader =
  stripAuthPrefix >=> eitherToMaybe . B64.decodeBase64 >=> splitOnColon
  where
    stripAuthPrefix s = do
      let (authType, creds) = B.break isSpace $ trimLeft s
      guard $ B.map toLower authType == "basic"
      pure $ trim creds
    trimLeft = B.dropWhile isSpace
    trim = trimRight . trimLeft
    trimRight = fst . B.spanEnd isSpace
    splitOnColon s = do
      let (login, p) = B.break (== ':') s
      (_, password) <- B.uncons p
      pure (login, password)

eitherToMaybe :: Either e a -> Maybe a
eitherToMaybe = either (const Nothing) Just
