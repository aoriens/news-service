module Core.Pagination.Test
  ( itShouldWorkWithPageSpecParserCorrectly
  ) where

import Core.Exception
import Core.Pagination
import Test.AsyncExpectation
import Test.Hspec

-- | Performs a test suite to check whether some code deals with
-- 'PageSpecParserHandle' correctly. Its parameter is the code to be
-- tested, which accepts parameters:
--
-- - 'PageSpecParserHandle' to use
-- - 'PageSpecQuery' to pass to the parser
-- - the success continuation to be passed the parser result into. It
--   should be invoked from within the ultimate code which uses the
--   'PageSpec', probably a stub data source.
itShouldWorkWithPageSpecParserCorrectly ::
     (PageSpecParserHandle -> PageSpecQuery -> (PageSpec -> IO ()) -> IO ())
  -> Spec
itShouldWorkWithPageSpecParserCorrectly test = do
  it "should pass parser result to the success continuation" $ do
    let expectedSpec =
          PageSpec {pageLimit = PageLimit 2, pageOffset = PageOffset 8}
        parserH = PageSpecParserHandle $ \_ -> Right expectedSpec
    shouldPassValue expectedSpec "The tested code" $ \pass -> do
      test parserH stubQuery pass
  it "should pass a PageSpecQuery to the parser" $ do
    let successSpec =
          PageSpec {pageLimit = PageLimit 8471, pageOffset = PageOffset 59126}
        query =
          PageSpecQuery {pageQueryLimit = Just 3, pageQueryOffset = Just 7}
        parserH =
          PageSpecParserHandle $ \q ->
            if q == query
              then Right successSpec
              else Left
                     "Incorrect PageSpecQuery is passed to the PageSpecParserHandle"
    shouldPassValue successSpec "The tested code" $ \pass -> do
      test parserH query pass
  it
    "should throw IncorrectParameterException if the parser handle returns Left _" $ do
    let parserH = PageSpecParserHandle $ \_ -> Left "an error"
        onSuccess spec =
          error $
          "The success continuation must not be invoked. The parameter: " ++
          show spec
    test parserH stubQuery onSuccess `shouldThrow` isIncorrectParameterException

stubQuery :: PageSpecQuery
stubQuery = PageSpecQuery {pageQueryOffset = Nothing, pageQueryLimit = Nothing}
