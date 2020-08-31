-- | Presenting and reading authentication credentials.
module Web.Credentials
  ( presentCredentials
  , readCredentials
  , getCredentialsFromRequest
  , Credentials(..)
  ) where

import Control.Monad.Catch
import qualified Core.Authentication as Core
import Core.DTO.User
import Data.ByteString.Base64
import qualified Data.ByteString.Char8 as B
import Data.Int.Exact
import Data.String
import qualified Network.Wai as Wai
import qualified Web.BasicAuth as BasicAuth
import Web.Exception

-- | Web-specific representation of core credentials. It depends from
-- the way of passing credentials via web.
data Credentials =
  LoginAndPassword B.ByteString B.ByteString
  deriving (Eq, Show)

presentCredentials :: Core.Credentials -> Credentials
presentCredentials (Core.TokenCredentials (UserId userIdent) (Core.SecretToken token)) =
  LoginAndPassword (fromString $ show userIdent) (encodeBase64' token)

readCredentials :: Credentials -> Maybe Core.Credentials
readCredentials (LoginAndPassword login password) = do
  userIdent <- readExactIntegral $ B.unpack login
  token <- either (const Nothing) Just $ decodeBase64 password
  pure $ Core.TokenCredentials (UserId userIdent) (Core.SecretToken token)

-- | Returns Nothing if no credentials found, but throws
-- 'BadRequestException' in case of malformed credentials.
getCredentialsFromRequest ::
     MonadThrow m => Wai.Request -> m (Maybe Core.Credentials)
getCredentialsFromRequest request =
  either (const failure) pure $ do
    optCreds <- BasicAuth.credentialsFromRequest request
    case optCreds of
      Nothing -> pure Nothing
      Just (login, password)
        | coreCreds@(Just _) <-
           readCredentials $ LoginAndPassword login password -> pure coreCreds
        | otherwise -> Left ()
  where
    failure = throwM $ BadRequestException "Malformed credentials"
