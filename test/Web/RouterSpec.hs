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
spec
  {- HLINT ignore spec "Reduce duplication" -}
 =
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
    it "should return a handler for path with prefix from ifPathPrefix argument" $ do
      let method = Http.methodGet
          prefix = ["a", "b"]
          expectedHandler = stubHandlerWithHeader ("X-My-Header", "")
          router =
            R.new $ R.ifPathPrefix prefix $ R.ifMethod method expectedHandler
          request =
            Wai.defaultRequest
              {Wai.pathInfo = prefix ++ ["x"], Wai.requestMethod = method}
          (R.HandlerResult handler) = R.route router request
      handler `shouldEmitSameHeadersAs` expectedHandler
    it "should return a handler for path equal to a prefix from ifPathPrefix" $ do
      let method = Http.methodGet
          prefix = ["a", "b"]
          expectedHandler = stubHandlerWithHeader ("X-My-Header", "")
          router =
            R.new $ R.ifPathPrefix prefix $ R.ifMethod method expectedHandler
          request =
            Wai.defaultRequest
              {Wai.pathInfo = prefix, Wai.requestMethod = method}
          (R.HandlerResult handler) = R.route router request
      handler `shouldEmitSameHeadersAs` expectedHandler
    it
      "when ifPathPrefix and ifPath both used with the same argument, \
      \ifPath should be selected for exact match" $ do
      let path = ["a", "b", "c"]
          expectedHandler = stubHandlerWithHeader ("X-My-Header", "")
          router =
            R.new $ do
              R.ifPathPrefix path $ R.ifMethod "GET" noOpHandler
              R.ifPath path $ R.ifMethod "GET" expectedHandler
          request = Wai.defaultRequest {Wai.pathInfo = path}
          (R.HandlerResult handler) = R.route router request
      handler `shouldEmitSameHeadersAs` expectedHandler
    it "should match the longest prefix in ifPathPrefix if no exact match found" $ do
      let prefix1 = ["a", "b"]
          prefix2 = prefix1 ++ ["c", "d"]
          path = prefix2 ++ ["q"]
          prefix3 = path ++ ["x"]
          expectedHandler = stubHandlerWithHeader ("X-My-Header", "")
          router =
            R.new $ do
              R.ifPathPrefix prefix1 $ R.ifMethod "GET" noOpHandler
              R.ifPathPrefix prefix2 $ R.ifMethod "GET" expectedHandler
              R.ifPathPrefix prefix3 $ R.ifMethod "GET" noOpHandler
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
              R.ifPath path $ R.ifMethod unneededMethod noOpHandler
              R.ifPathPrefix prefix $ R.ifMethod neededMethod noOpHandler
          request =
            Wai.defaultRequest
              {Wai.pathInfo = path, Wai.requestMethod = neededMethod}
          result = R.route router request
      result `shouldSatisfy` R.isMethodNotSupportedResult
    it
      "when ifPathPrefix matched, should pass to handler pathInfo with the prefix removed" $ do
      pathInfoRef <- newIORef $ error "pathInfo must have been stored here"
      let method = Http.methodGet
          prefix = ["a", "b"]
          suffix = ["y", "z"]
          expectedHandler _ r _ = do
            writeIORef pathInfoRef $ Wai.pathInfo r
            pure Wai.ResponseReceived
          router =
            R.new $ R.ifPathPrefix prefix $ R.ifMethod method expectedHandler
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
              R.ifAppURL $ \U.URLImage {} -> R.ifMethod "GET" noOpHandler
              R.ifPath ["path"] $ R.ifMethod "GET" noOpHandler
              R.ifPathPrefix ["prefix"] $ R.ifMethod "GET" noOpHandler
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
