-- | Presenting and reading authentication credentials.
module Web.Credentials
  ( presentCredentials
  , readCredentials
  , Credentials(..)
  ) where

import qualified Core.Authentication as Auth
import Core.DTO.User
import Data.String
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.ByteString.Base64 as B64
import Data.Int.Exact
import qualified Network.Wai as Wai

data Credentials =
  LoginAndPassword T.Text T.Text

presentCredentials :: Auth.Credentials -> Credentials
presentCredentials (Auth.TokenCredentials (UserId userIdent) (Auth.SecretToken token)) =
  LoginAndPassword (fromString $ show userIdent) (B64.encodeBase64 token)

readCredentials :: Credentials -> Maybe Auth.Credentials
readCredentials (LoginAndPassword login password) = do
  userIdent <- readExactIntegral (T.unpack login)
  token <-
    either (const Nothing) Just . B64.decodeBase64 $ T.encodeUtf8 password
  pure $ Auth.TokenCredentials (UserId userIdent) (Auth.SecretToken token)

credentialsFromRequest :: Wai.Request -> m (Maybe Auth.Credentials)
credentialsFromRequest request = undefined
