module Web.BasicAuth
  ( basicAuthCredentialsFromRequest
  ) where

import qualified Network.Wai as Wai
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Network.HTTP.Types as Http
import qualified Data.ByteString.Char8 as BS
import Data.Char
import Control.Monad
import Data.ByteString.Base64 as B64

basicAuthCredentialsFromRequest :: Wai.Request -> Maybe (T.Text, T.Text)
basicAuthCredentialsFromRequest =
  lookup Http.hAuthorization . Wai.requestHeaders >=>
  stripAuthPrefix >=> eitherToMaybe . B64.decodeBase64 >=> splitOnColon
  where
    stripAuthPrefix s = do
      let (authType, creds) = BS.break isSpace $ trimLeft s
      guard $ BS.map toLower authType == "basic"
      pure $ trim creds
    trimLeft = BS.dropWhile isSpace
    trim = trimRight . trimLeft
    trimRight = fst . BS.spanEnd isSpace
    splitOnColon s = do
      text <- eitherToMaybe $ T.decodeUtf8' s
      let (login, p) = T.break (== ':') text
      (_, password) <- T.uncons p
      pure (login, password)

eitherToMaybe :: Either e a -> Maybe a
eitherToMaybe = either (const Nothing) Just
