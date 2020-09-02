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
import Data.ByteString.Base64
import qualified Data.ByteString.Char8 as B
import Data.Either.Util
import Data.Integral.Exact
import Data.String
import qualified Network.Wai as Wai
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

readCredentials :: Credentials -> Maybe Core.Credentials
readCredentials (WebToken webToken) = do
  let (uidString, t) = B.break (== ',') webToken
  userIdent <- readExactIntegral $ B.unpack uidString
  (_, codedToken) <- B.uncons t
  token <- eitherToMaybe $ decodeBase64 codedToken
  pure $ Core.TokenCredentials (UserId userIdent) (Core.SecretToken token)

-- | Returns Nothing if no credentials found, but throws
-- 'MalformedAuthDataException in case of malformed credentials.
getCredentialsFromRequest ::
     MonadThrow m => Wai.Request -> m (Maybe Core.Credentials)
getCredentialsFromRequest request =
  either (const failure) pure $ do
    optCreds <- BasicAuth.credentialsFromRequest request
    case optCreds of
      Nothing -> pure Nothing
      Just (login, _)
        | coreCreds@(Just _) <- readCredentials $ WebToken login ->
          pure coreCreds
        | otherwise -> Left ""
  where
    failure = throwM MalformedAuthDataException
