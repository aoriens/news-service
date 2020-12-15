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
      (commands, h) <- handleWith env
      run h someNonAdminUser Nothing names `shouldThrow` isNoPermissionException
      readIORef commands `shouldReturn` []
    it
      "should throw NoPermissionException if the user is not an admin even if a name is empty" $ do
      let names = "" :| []
      (commands, h) <- handleWith env
      run h someNonAdminUser Nothing names `shouldThrow` isNoPermissionException
      readIORef commands `shouldReturn` []
    it "should create category if all names are non-empty and the user is admin" $ do
      let names = "1" :| ["2"]
          parentId = Just $ CategoryId 1
          expectedCat = stubCategory {categoryName = "some name"}
      (commands, h) <-
        handleWith env {envCreateCategoryResult = Right expectedCat}
      r <- run h someAdminUser parentId names
      r `shouldBe` Right expectedCat
      readIORef commands `shouldReturn` [(parentId, names)]
    it
      "should return UnknownParentCategoryId if the gateway returns CCFUnknownParentCategoryId" $ do
      let parentId = Just $ CategoryId 0
          names = "a" :| ["b"]
      (_, h) <-
        handleWith
          env {envCreateCategoryResult = Left CCFUnknownParentCategoryId}
      r <- run h someAdminUser parentId names
      r `shouldBe` Left UnknownParentCategoryId
    it
      "should return CategoryNameMustNotBeEmpty if the least significant category name is empty" $ do
      let parentId = Nothing
          names = "" :| []
      (commands, h) <- handleWith env
      r <- run h someAdminUser parentId names
      r `shouldBe` Left CategoryNameMustNotBeEmpty
      readIORef commands `shouldReturn` []
    it
      "should return CategoryNameMustNotBeEmpty if a non-least significant category name is empty" $ do
      let parentId = Nothing
          names = "nonempty" :| [""]
      (commands, h) <- handleWith env
      r <- run h someAdminUser parentId names
      r `shouldBe` Left CategoryNameMustNotBeEmpty
      readIORef commands `shouldReturn` []
    it
      "should return CategoryNameMustBeUniqueAmongSiblings if the top new \
      \category name is not unique among its future siblings and the parent is Just _" $ do
      let parentId = Just $ CategoryId 1
          names = "root" :| ["nested"]
      (commands, h) <-
        handleWith env {envKnownChildrenNames = (parentId, ["root"])}
      r <- run h someAdminUser parentId names
      r `shouldBe` Left CategoryNameMustBeUniqueAmongSiblings
      readIORef commands `shouldReturn` []

type CreateCategoryCommandLog = [(Maybe CategoryId, NonEmpty T.Text)]

data Env =
  Env
    { envCreateCategoryResult :: Either CreateCategoryFailure Category
    , envKnownChildrenNames :: (Maybe CategoryId, [T.Text])
    }

-- | A default, empty environment
env :: Env
env =
  Env
    { envCreateCategoryResult = Right stubCategory
    , envKnownChildrenNames = (Nothing, [])
    }

handleWith :: Env -> IO (IORef CreateCategoryCommandLog, Handle IO)
handleWith Env {..} = do
  logRef <- newIORef []
  let handle =
        Handle
          { hCreateCategory =
              \optParentId names ->
                updateIORef' logRef $ \commands ->
                  (commands ++ [(optParentId, names)], envCreateCategoryResult)
          , hCategoryIdWithParentAndNameExists =
              \optParentId name ->
                let (knownParentId, names) = envKnownChildrenNames
                 in pure $ knownParentId == optParentId && name `elem` names
          }
  pure (logRef, handle)
