module Web.RouterSpec
  ( spec
  ) where

import Data.IORef
import Data.List
import qualified Network.HTTP.Types as Http
import qualified Network.Wai as Wai
import qualified Network.Wai.Internal as Wai
import Test.Hspec
import qualified Web.Router as R

spec :: Spec
spec =
  describe "route" $ do
    it "should return a handler by URL path and HTTP method" $ do
      let path = ["my_path"]
          method = Http.methodPost
          expectedHeader = ("X-My-Header", "")
          expectedHandler _ = ($ Wai.responseLBS Http.ok200 [expectedHeader] "")
          router = R.new $ R.ifPath path $ R.ifMethod method expectedHandler
          request =
            Wai.defaultRequest {Wai.pathInfo = path, Wai.requestMethod = method}
          (R.HandlerResult handler) = R.route router request
      (_, headers) <- getResponse handler request
      headers `shouldContain` [expectedHeader]
    it "should return ResourceNotFoundRequest for unknown URL path" $ do
      let router = R.new $ pure ()
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

getResponse ::
     Wai.Application -> Wai.Request -> IO (Http.Status, Http.ResponseHeaders)
getResponse app request = do
  result <- newIORef (error "The response continuation must be invoked")
  _ <-
    app request $ \response -> do
      let (status, headers, _) = Wai.responseToStream response
      writeIORef result (status, headers)
      pure Wai.ResponseReceived
  readIORef result

noOpHandler :: Wai.Application
noOpHandler _ = ($ Wai.responseLBS Http.ok200 [] "")
