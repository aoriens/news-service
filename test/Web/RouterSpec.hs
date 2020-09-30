module Web.RouterSpec
  ( spec
  ) where

import Core.Image
import Data.IORef
import Data.List
import qualified Network.HTTP.Types as Http
import qualified Network.Wai.Internal as Wai
import Test.Hspec
import qualified Web.AppURI as U
import qualified Web.Router as R
import Web.Types
import Web.Types.Internal.SessionId as Web

spec :: Spec
spec
  {- HLINT ignore spec "Reduce duplication" -}
 =
  describe "route" $ do
    it
      "should return a handler matching an AppURI if an AppURI-decodable path is passed" $ do
      let appURI = U.ImageURI (ImageId 1)
          method = Http.methodGet
          expectedHandler = stubHandlerWithHeader ("X-My-Header", "")
          router = R.new $ \U.ImageURI {} -> R.method method expectedHandler
          request =
            defaultRequest
              { pathInfo = U.relativeURIPath $ U.toRelativeURI appURI
              , requestMethod = method
              }
          (R.HandlerResult handler) = R.route router request
      handler `shouldEmitSameHeadersAs` expectedHandler
    it "should return ResourceNotFoundRequest for an empty router" $ do
      let router = R.new $ \U.ImageURI {} -> pure ()
          request = defaultRequest {pathInfo = ["unknown_path"]}
          result = R.route router request
      result `shouldSatisfy` R.isResourceNotFoundResult
    it "should return ResourceNotFoundRequest if no match found" $ do
      let router = R.new $ \U.ImageURI {} -> R.method "GET" noOpHandler
          request = defaultRequest {pathInfo = ["unknown_path"]}
          result = R.route router request
      result `shouldSatisfy` R.isResourceNotFoundResult
    it
      "should return MethodNotSupportedResult with sorted known methods for known path but unknown method" $ do
      let uri = U.ImageURI (ImageId 0)
          U.RelativeURI path = U.toRelativeURI uri
          unknownMethod = "UNKNOWN"
          router =
            R.new $ \U.ImageURI {} -> do
              R.method Http.methodPost noOpHandler
              R.method Http.methodPut noOpHandler
              R.method Http.methodDelete noOpHandler
          request =
            defaultRequest {pathInfo = path, requestMethod = unknownMethod}
          result@(R.MethodNotSupportedResult methods) = R.route router request
      result `shouldSatisfy` R.isMethodNotSupportedResult
      methods `shouldBe`
        sort [Http.methodPost, Http.methodPut, Http.methodDelete]

stubHandlerWithHeader :: Http.Header -> EApplication
stubHandlerWithHeader header _ _ = ($ responseLBS Http.ok200 [header] "")

shouldEmitSameHeadersAs ::
     HasCallStack => EApplication -> EApplication -> Expectation
shouldEmitSameHeadersAs a1 a2 = do
  headers1 <- getHeaders (a1 stubSession) defaultRequest
  headers2 <- getHeaders (a2 stubSession) defaultRequest
  headers1 `shouldBe` headers2

getHeaders :: Application -> Request -> IO Http.ResponseHeaders
getHeaders app request = do
  result <- newIORef (error "The response continuation must be invoked")
  _ <-
    app request $ \response -> do
      let (_, headers, _) = responseToStream response
      writeIORef result headers
      pure Wai.ResponseReceived
  readIORef result

noOpHandler :: EApplication
noOpHandler _ _ = ($ responseLBS Http.ok200 [] "")

stubSession :: Session
stubSession = Session {sessionId = SessionId 0}
