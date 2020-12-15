module Core.Interactor.CreateCategorySpec
  ( spec
  ) where

import Core.Authentication.Test
import Core.Category
import Core.Exception
import Core.Interactor.CreateCategory
import Core.Stubs
import Data.IORef
import Data.IORef.Util
import Data.List.NonEmpty
import qualified Data.Text as T
import Test.Hspec

spec :: Spec
spec
  {- HLINT ignore spec "Reduce duplication" -}
 =
  describe "run" $ do
    it "should throw NoPermissionException if the user is not an admin" $ do
      let names = "name" :| []
      (commands, h) <- handleWithSomeResult
      run h someNonAdminUser Nothing names `shouldThrow` isNoPermissionException
      readIORef commands `shouldReturn` []
    it
      "should throw NoPermissionException if the user is not an admin even if a name is empty" $ do
      let names = "" :| []
      (commands, h) <- handleWithSomeResult
      run h someNonAdminUser Nothing names `shouldThrow` isNoPermissionException
      readIORef commands `shouldReturn` []
    it "should create category if all names are non-empty and the user is admin" $ do
      let names = "1" :| ["2"]
          parentId = Just $ CategoryId 1
          expectedCat = stubCategory {categoryName = "some name"}
      (commands, h) <- handleWithResult $ Right expectedCat
      r <- run h someAdminUser parentId names
      r `shouldBe` Right expectedCat
      readIORef commands `shouldReturn` [(parentId, names)]
    it
      "should return UnknownParentCategoryId if the gateway returns CCFUnknownParentCategoryId" $ do
      let parentId = Just $ CategoryId 0
          names = "a" :| ["b"]
      (_, h) <- handleWithResult $ Left CCFUnknownParentCategoryId
      r <- run h someAdminUser parentId names
      r `shouldBe` Left UnknownParentCategoryId
    it
      "should return CategoryNameMustNotBeEmpty if the least significant category name is empty" $ do
      let parentId = Nothing
          names = "" :| []
      (commands, h) <- handleWithSomeResult
      r <- run h someAdminUser parentId names
      r `shouldBe` Left CategoryNameMustNotBeEmpty
      readIORef commands `shouldReturn` []
    it
      "should return CategoryNameMustNotBeEmpty if a non-least significant category name is empty" $ do
      let parentId = Nothing
          names = "nonempty" :| [""]
      (commands, h) <- handleWithSomeResult
      r <- run h someAdminUser parentId names
      r `shouldBe` Left CategoryNameMustNotBeEmpty
      readIORef commands `shouldReturn` []

type StorageCommandLog = [(Maybe CategoryId, NonEmpty T.Text)]

handleWithResult ::
     Either CreateCategoryFailure Category
  -> IO (IORef StorageCommandLog, Handle IO)
handleWithResult result = do
  logRef <- newIORef []
  let handle =
        Handle $ \optParentId names ->
          updateIORef' logRef $ \commands ->
            (commands ++ [(optParentId, names)], result)
  pure (logRef, handle)

handleWithSomeResult :: IO (IORef StorageCommandLog, Handle IO)
handleWithSomeResult = handleWithResult $ Right stubCategory
