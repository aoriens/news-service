module Web.RouterSpec
  ( spec
  ) where

import Core.DTO.Image
import Data.IORef
import Data.List
import qualified Network.HTTP.Types as Http
import qualified Network.Wai as Wai
import qualified Network.Wai.Internal as Wai
import Test.Hspec
import qualified Web.AppURL as U
import qualified Web.Router as R
import Web.Types as Web
import Web.Types.Internal.SessionId as Web

spec :: Spec
spec =
  describe "route" $ do
    it "should return a handler by URL path and HTTP method" $ do
      let path = ["my_path"]
          method = Http.methodPost
          expectedHandler = stubHandlerWithHeader ("X-My-Header", "")
          router = R.new $ R.ifPath path $ R.ifMethod method expectedHandler
          request =
            Wai.defaultRequest {Wai.pathInfo = path, Wai.requestMethod = method}
          (R.HandlerResult handler) = R.route router request
      handler `shouldEmitSameHeadersAs` expectedHandler
    it
      "should return a handler matching an AppURL if an AppURL-decodable path is passed" $ do
      let appURL = U.URLImage (ImageId 1)
          method = Http.methodGet
          expectedHandler = stubHandlerWithHeader ("X-My-Header", "")
          router =
            R.new $
            R.ifAppURL $ \U.URLImage {} -> R.ifMethod method expectedHandler
          request =
            Wai.defaultRequest
              { Wai.pathInfo = U.relativeURLPath $ U.toRelativeURL appURL
              , Wai.requestMethod = method
              }
          (R.HandlerResult handler) = R.route router request
      handler `shouldEmitSameHeadersAs` expectedHandler
    it "should return ResourceNotFoundRequest for unknown URL path" $ do
      let router = R.new $ pure ()
          request = Wai.defaultRequest {Wai.pathInfo = ["unknown_path"]}
          result = R.route router request
      result `shouldSatisfy` R.isResourceNotFoundResult
    it
      "should return ResourceNotFoundRequest for unknown URL path even when ifAppURL is used" $ do
      let router =
            R.new $ R.ifAppURL $ \U.URLImage {} -> R.ifMethod "GET" noOpHandler
          request = Wai.defaultRequest {Wai.pathInfo = ["unknown_path"]}
          result = R.route router request
      result `shouldSatisfy` R.isResourceNotFoundResult
    it
      "should return MethodNotSupportedResult with sorted known methods for known path but unknown method" $ do
      let path = ["my_path"]
          unknownMethod = Http.methodGet
          router =
            R.new . R.ifPath path $ do
              R.ifMethod Http.methodPost noOpHandler
              R.ifMethod Http.methodPut noOpHandler
              R.ifMethod Http.methodDelete noOpHandler
          request =
            Wai.defaultRequest
              {Wai.pathInfo = path, Wai.requestMethod = unknownMethod}
          result@(R.MethodNotSupportedResult methods) = R.route router request
      result `shouldSatisfy` R.isMethodNotSupportedResult
      methods `shouldBe`
        sort [Http.methodPost, Http.methodPut, Http.methodDelete]

stubHandlerWithHeader :: Http.Header -> EApplication
stubHandlerWithHeader header _ _ = ($ Wai.responseLBS Http.ok200 [header] "")

shouldEmitSameHeadersAs ::
     HasCallStack => EApplication -> EApplication -> Expectation
shouldEmitSameHeadersAs a1 a2 = do
  headers1 <- getHeaders (a1 stubSession) Wai.defaultRequest
  headers2 <- getHeaders (a2 stubSession) Wai.defaultRequest
  headers1 `shouldBe` headers2

getHeaders :: Wai.Application -> Wai.Request -> IO Http.ResponseHeaders
getHeaders app request = do
  result <- newIORef (error "The response continuation must be invoked")
  _ <-
    app request $ \response -> do
      let (_, headers, _) = Wai.responseToStream response
      writeIORef result headers
      pure Wai.ResponseReceived
  readIORef result

noOpHandler :: Web.EApplication
noOpHandler _ _ = ($ Wai.responseLBS Http.ok200 [] "")

stubSession :: Web.Session
stubSession = Web.Session {sessionId = Web.SessionId 0}
