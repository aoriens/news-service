module Web.RouterSpec
  ( spec
  ) where

import Core.Image
import Data.IORef
import Data.List
import qualified Network.HTTP.Types as Http
import qualified Network.Wai as Wai
import qualified Network.Wai.Internal as Wai
import Test.Hspec
import qualified Web.AppURI as U
import qualified Web.Router as R
import Web.Types as Web
import Web.Types.Internal.SessionId as Web

spec :: Spec
spec
  {- HLINT ignore spec "Reduce duplication" -}
 =
  describe "route" $ do
    it "should return a handler by URI path and HTTP method" $ do
      let path = ["my_path"]
          method = Http.methodPost
          expectedHandler = stubHandlerWithHeader ("X-My-Header", "")
          router = R.new $ R.path path $ R.method method expectedHandler
          request =
            Wai.defaultRequest {Wai.pathInfo = path, Wai.requestMethod = method}
          (R.HandlerResult handler) = R.route router request
      handler `shouldEmitSameHeadersAs` expectedHandler
    it
      "should return a handler matching an AppURI if an AppURI-decodable path is passed" $ do
      let appURI = U.ImageURI (ImageId 1)
          method = Http.methodGet
          expectedHandler = stubHandlerWithHeader ("X-My-Header", "")
          router =
            R.new $ R.appURI $ \U.ImageURI {} -> R.method method expectedHandler
          request =
            Wai.defaultRequest
              { Wai.pathInfo = U.relativeURIPath $ U.toRelativeURI appURI
              , Wai.requestMethod = method
              }
          (R.HandlerResult handler) = R.route router request
      handler `shouldEmitSameHeadersAs` expectedHandler
    it "should return a handler for path with prefix from pathPrefix argument" $ do
      let method = Http.methodGet
          prefix = ["a", "b"]
          expectedHandler = stubHandlerWithHeader ("X-My-Header", "")
          router = R.new $ R.pathPrefix prefix $ R.method method expectedHandler
          request =
            Wai.defaultRequest
              {Wai.pathInfo = prefix ++ ["x"], Wai.requestMethod = method}
          (R.HandlerResult handler) = R.route router request
      handler `shouldEmitSameHeadersAs` expectedHandler
    it "should return a handler for path equal to a prefix from pathPrefix" $ do
      let method = Http.methodGet
          prefix = ["a", "b"]
          expectedHandler = stubHandlerWithHeader ("X-My-Header", "")
          router = R.new $ R.pathPrefix prefix $ R.method method expectedHandler
          request =
            Wai.defaultRequest
              {Wai.pathInfo = prefix, Wai.requestMethod = method}
          (R.HandlerResult handler) = R.route router request
      handler `shouldEmitSameHeadersAs` expectedHandler
    it
      "when pathPrefix and path both used with the same argument, \
      \path should be selected for exact match" $ do
      let path = ["a", "b", "c"]
          expectedHandler = stubHandlerWithHeader ("X-My-Header", "")
          router =
            R.new $ do
              R.pathPrefix path $ R.method "GET" noOpHandler
              R.path path $ R.method "GET" expectedHandler
          request = Wai.defaultRequest {Wai.pathInfo = path}
          (R.HandlerResult handler) = R.route router request
      handler `shouldEmitSameHeadersAs` expectedHandler
    it "should match the longest prefix in pathPrefix if no exact match found" $ do
      let prefix1 = ["a", "b"]
          prefix2 = prefix1 ++ ["c", "d"]
          path = prefix2 ++ ["q"]
          prefix3 = path ++ ["x"]
          expectedHandler = stubHandlerWithHeader ("X-My-Header", "")
          router =
            R.new $ do
              R.pathPrefix prefix1 $ R.method "GET" noOpHandler
              R.pathPrefix prefix2 $ R.method "GET" expectedHandler
              R.pathPrefix prefix3 $ R.method "GET" noOpHandler
          request = Wai.defaultRequest {Wai.pathInfo = path}
          (R.HandlerResult handler) = R.route router request
      handler `shouldEmitSameHeadersAs` expectedHandler
    it
      "should return MethodNotSupportedResult when path matches exactly, but \
       \method does not, and there is a prefix match containing the required method" $ do
      let neededMethod = Http.methodGet
          unneededMethod = Http.methodPost
          prefix = ["a", "b"]
          path = prefix ++ ["z"]
          router =
            R.new $ do
              R.path path $ R.method unneededMethod noOpHandler
              R.pathPrefix prefix $ R.method neededMethod noOpHandler
          request =
            Wai.defaultRequest
              {Wai.pathInfo = path, Wai.requestMethod = neededMethod}
          result = R.route router request
      result `shouldSatisfy` R.isMethodNotSupportedResult
    it
      "when pathPrefix matched, should pass to handler pathInfo with the prefix removed" $ do
      pathInfoRef <- newIORef $ error "pathInfo must have been stored here"
      let method = Http.methodGet
          prefix = ["a", "b"]
          suffix = ["y", "z"]
          expectedHandler _ r _ = do
            writeIORef pathInfoRef $ Wai.pathInfo r
            pure Wai.ResponseReceived
          router = R.new $ R.pathPrefix prefix $ R.method method expectedHandler
          request =
            Wai.defaultRequest
              {Wai.pathInfo = prefix ++ suffix, Wai.requestMethod = method}
          (R.HandlerResult handler) = R.route router request
      Wai.ResponseReceived <- handler undefined request undefined
      readIORef pathInfoRef `shouldReturn` suffix
    it "should return ResourceNotFoundRequest for an empty router" $ do
      let router = R.new $ pure ()
          request = Wai.defaultRequest {Wai.pathInfo = ["unknown_path"]}
          result = R.route router request
      result `shouldSatisfy` R.isResourceNotFoundResult
    it "should return ResourceNotFoundRequest if no match found" $ do
      let router =
            R.new $ do
              R.appURI $ \U.ImageURI {} -> R.method "GET" noOpHandler
              R.path ["path"] $ R.method "GET" noOpHandler
              R.pathPrefix ["prefix"] $ R.method "GET" noOpHandler
          request = Wai.defaultRequest {Wai.pathInfo = ["unknown_path"]}
          result = R.route router request
      result `shouldSatisfy` R.isResourceNotFoundResult
    it
      "should return MethodNotSupportedResult with sorted known methods for known path but unknown method" $ do
      let path = ["my_path"]
          unknownMethod = Http.methodGet
          router =
            R.new . R.path path $ do
              R.method Http.methodPost noOpHandler
              R.method Http.methodPut noOpHandler
              R.method Http.methodDelete noOpHandler
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
