module Web.RouterSpec
  ( spec
  ) where

import Core.Image
import Data.IORef
import Data.List
import qualified Network.HTTP.Types as Http
import Test.Hspec
import qualified Web.AppURI as U
import Web.Application
import Web.Application.Internal.ResponseReceived
import Web.Application.Internal.SessionId as Web
import qualified Web.Router as R

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
          router =
            R.new $ \case
              U.ImageURI {} -> [R.method method expectedHandler]
              _ -> []
          request =
            defaultRequest
              { requestPathInfo = U.relativeURIPath $ U.toRelativeURI appURI
              , requestMethod = method
              }
          result = R.route router request
          handler =
            case result of
              R.HandlerResult h -> h
              _ -> error "UnexpectedResult"
      handler `shouldEmitSameHeadersAs` expectedHandler
    it "should return ResourceNotFoundRequest for an empty router" $ do
      let router = R.new $ const []
          request = defaultRequest {requestPathInfo = ["unknown_path"]}
          result = R.route router request
      result `shouldSatisfy` R.isResourceNotFoundResult
    it "should return ResourceNotFoundRequest if no match found" $ do
      let router =
            R.new $ \case
              U.ImageURI {} -> [R.method "GET" noOpHandler]
              _ -> []
          request = defaultRequest {requestPathInfo = ["unknown_path"]}
          result = R.route router request
      result `shouldSatisfy` R.isResourceNotFoundResult
    it
      "should return ResourceNotFoundRequest if a URI match is found, but no methods available" $ do
      let uri = U.ImageURI (ImageId 0)
          U.RelativeURI path = U.toRelativeURI uri
          router =
            R.new $ \case
              U.ImageURI {} -> []
              _ -> []
          request = defaultRequest {requestPathInfo = path}
          result = R.route router request
      result `shouldSatisfy` R.isResourceNotFoundResult
    it
      "should return MethodNotSupportedResult with sorted known methods for known path but unknown method" $ do
      let uri = U.ImageURI (ImageId 0)
          U.RelativeURI path = U.toRelativeURI uri
          unknownMethod = "UNKNOWN"
          router =
            R.new $ \case
              U.ImageURI {} ->
                [ R.method Http.methodPost noOpHandler
                , R.method Http.methodPut noOpHandler
                , R.method Http.methodDelete noOpHandler
                ]
              _ -> []
          request =
            defaultRequest
              {requestPathInfo = path, requestMethod = unknownMethod}
          result = R.route router request
          methods =
            case result of
              R.MethodNotSupportedResult m -> m
              _ -> error "Unexpected result"
      result `shouldSatisfy` R.isMethodNotSupportedResult
      methods `shouldBe`
        sort [Http.methodPost, Http.methodPut, Http.methodDelete]

stubHandlerWithHeader :: Http.Header -> EApplication
stubHandlerWithHeader header _ _ =
  ($ responseBuilder Http.ok200 [header] mempty)

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
      let (_, headers) = responseStatusAndHeaders response
      writeIORef result headers
      pure ResponseReceived
  readIORef result

noOpHandler :: EApplication
noOpHandler _ _ = ($ responseBuilder Http.ok200 [] mempty)

stubSession :: Session
stubSession = Session {sessionId = SessionId 0}
