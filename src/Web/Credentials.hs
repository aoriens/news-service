-- | Presenting and reading authentication credentials.
module Web.Credentials
  ( presentCredentials
  , readCredentials
  , getCredentialsFromRequest
  , Credentials(..)
  ) where

import Control.Monad.Catch
import qualified Core.Authentication as Core
import Core.User
import Data.Bifunctor
import Data.ByteString.Base64
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Util as B
import Data.Either.Util
import Data.Integral.Exact
import Data.String
import qualified Data.Text as T
import Web.Application
import qualified Web.BasicAuth as BasicAuth
import Web.Exception

-- | Web-specific representation of core credentials. It depends from
-- the way of passing credentials via web.
newtype Credentials =
  WebToken
    { unWebToken :: B.ByteString
    }
  deriving (Eq, Show)

presentCredentials :: Core.Credentials -> Credentials
presentCredentials (Core.TokenCredentials (UserId userIdent) (Core.SecretToken token)) =
  WebToken $ fromString (show userIdent) <> "," <> encodeBase64' token

readCredentials :: Credentials -> Either T.Text Core.Credentials
readCredentials (WebToken webToken) = do
  (uidString, codedToken) <-
    maybeToEither "comma is missing" $ B.splitOnCharOnce (== ',') webToken
  userIdent <-
    maybeToEither "user id is malformed or too large" $
    readExactIntegral (B.unpack uidString)
  token <- first ("incorrect base64 in secret: " <>) $ decodeBase64 codedToken
  pure $ Core.TokenCredentials (UserId userIdent) (Core.SecretToken token)

-- | Returns Nothing if no credentials found, but throws
-- 'MalformedAuthDataException in case of malformed credentials.
getCredentialsFromRequest ::
     MonadThrow m => Request -> m (Maybe Core.Credentials)
getCredentialsFromRequest request =
  either (throwM . MalformedAuthDataException) pure $ do
    optCreds <-
      first ("in basic auth: " <>) $ BasicAuth.credentialsFromRequest request
    case optCreds of
      Nothing -> pure Nothing
      Just (login, _) ->
        bimap ("in the web token: " <>) Just $ readCredentials (WebToken login)
