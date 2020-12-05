{-# LANGUAGE TupleSections #-}

module Core.Interactor.DeleteCategorySpec
  ( spec
  ) where

import Core.Authentication.Test
import Core.Category
import Core.Exception
import Core.Interactor.DeleteCategory
import Data.Functor
import qualified Data.HashSet as Set
import Data.IORef
import Data.List
import Test.Hspec

spec :: Spec
spec
  {- HLINT ignore spec "Reduce duplication" -}
 =
  describe "run" $ do
    it
      "should throw NoPermissionException and do not change data if the user is not an admin" $ do
      let existingCatId = CategoryId 1
          initialData =
            storage [[existingCatId]] [StubDraft $ Just existingCatId]
      db <- newIORef initialData
      run (handleWith db) someNonAdminUser existingCatId `shouldThrow`
        isNoPermissionException
      readIORef db `shouldReturn` initialData
    it
      "should return Left UnknownCategoryId and keep data untouched if the user is admin and no category is found" $ do
      let existingCatId = CategoryId 1
          missingCatId = CategoryId 2
          initialData =
            storage [[existingCatId]] [StubDraft $ Just existingCatId]
      db <- newIORef initialData
      r <- run (handleWith db) someAdminUser missingCatId
      r `shouldBe` Left UnknownCategoryId
      readIORef db `shouldReturn` initialData
    it
      "should return Right () and delete an existing category and all the descendants if the user is admin" $ do
      let initialData =
            storage
              [ [CategoryId 1, CategoryId 2, CategoryId 3]
              , [CategoryId 1, CategoryId 2, CategoryId 4, CategoryId 5]
              , [CategoryId 6, CategoryId 7]
              ]
              []
      db <- newIORef initialData
      r <- run (handleWith db) someAdminUser (CategoryId 2)
      r `shouldBe` Right ()
      finalData <- readIORef db
      storageCategories finalData `shouldMatchList`
        categoryMap [[CategoryId 1], [CategoryId 6, CategoryId 7]]
      storageRequestedCategoryDeletions finalData `shouldBe`
        Set.fromList [CategoryId 2, CategoryId 3, CategoryId 4, CategoryId 5]
    it
      "should move all drafts from the deleted non-root category and its descendants to its parent if the user is admin" $ do
      let initialData =
            storage
              [ [CategoryId 1, CategoryId 2, CategoryId 3]
              , [CategoryId 1, CategoryId 2, CategoryId 4, CategoryId 5]
              , [CategoryId 6, CategoryId 7]
              ]
              [ StubDraft $ Just $ CategoryId 1
              , StubDraft $ Just $ CategoryId 2
              , StubDraft $ Just $ CategoryId 3
              , StubDraft $ Just $ CategoryId 4
              , StubDraft $ Just $ CategoryId 5
              , StubDraft $ Just $ CategoryId 6
              , StubDraft $ Just $ CategoryId 7
              , StubDraft Nothing
              ]
      db <- newIORef initialData
      _ <- run (handleWith db) someAdminUser (CategoryId 2)
      finalData <- readIORef db
      storageRequestedDraftCategoryChanges finalData `shouldBe`
        Set.fromList
          [ (CategoryId 2, Just $ CategoryId 1)
          , (CategoryId 3, Just $ CategoryId 1)
          , (CategoryId 4, Just $ CategoryId 1)
          , (CategoryId 5, Just $ CategoryId 1)
          ]
      storageDrafts finalData `shouldMatchList`
        [ StubDraft $ Just $ CategoryId 1
        , StubDraft $ Just $ CategoryId 1
        , StubDraft $ Just $ CategoryId 1
        , StubDraft $ Just $ CategoryId 1
        , StubDraft $ Just $ CategoryId 1
        , StubDraft $ Just $ CategoryId 6
        , StubDraft $ Just $ CategoryId 7
        , StubDraft Nothing
        ]
    it
      "should move all drafts from the deleted root category and its descendants to Nothing category if the user is admin" $ do
      let initialData =
            storage
              [ [CategoryId 1]
              , [CategoryId 1, CategoryId 2, CategoryId 3]
              , [CategoryId 4, CategoryId 5]
              ]
              [ StubDraft $ Just $ CategoryId 1
              , StubDraft $ Just $ CategoryId 2
              , StubDraft $ Just $ CategoryId 3
              , StubDraft $ Just $ CategoryId 4
              , StubDraft $ Just $ CategoryId 5
              , StubDraft Nothing
              ]
      db <- newIORef initialData
      _ <- run (handleWith db) someAdminUser (CategoryId 1)
      finalData <- readIORef db
      storageRequestedDraftCategoryChanges finalData `shouldBe`
        Set.fromList
          [ (CategoryId 1, Nothing)
          , (CategoryId 2, Nothing)
          , (CategoryId 3, Nothing)
          ]
      storageDrafts finalData `shouldMatchList`
        [ StubDraft Nothing
        , StubDraft Nothing
        , StubDraft Nothing
        , StubDraft $ Just $ CategoryId 4
        , StubDraft $ Just $ CategoryId 5
        , StubDraft Nothing
        ]

data Storage =
  Storage
    { storageCategories :: CategoryMap
    , storageDrafts :: [StubDraft]
    , storageRequestedCategoryDeletions :: Set.HashSet CategoryId
    , storageRequestedDraftCategoryChanges :: Set.HashSet ( CategoryId
                                                          , Maybe CategoryId)
    }
  deriving (Eq, Show)

-- | List-based category connectivity map: child->parent
type CategoryMap = [(CategoryId, Maybe CategoryId)]

-- | A stub analogue of a category - a list of nested categories in
-- the parent to child order.
type StubCategory = [CategoryId]

newtype StubDraft =
  StubDraft
    { draftCategory :: Maybe CategoryId
    }
  deriving (Eq, Show)

storage :: [StubCategory] -> [StubDraft] -> Storage
storage cats drafts =
  Storage
    { storageCategories = categoryMap cats
    , storageDrafts = drafts
    , storageRequestedCategoryDeletions = Set.empty
    , storageRequestedDraftCategoryChanges = Set.empty
    }

categoryMap :: [StubCategory] -> CategoryMap
categoryMap = nub . concatMap generateMapForCategory
  where
    generateMapForCategory [] = []
    generateMapForCategory cs = zip cs $ Nothing : map Just cs

handleWith :: IORef Storage -> Handle IO
handleWith ref =
  Handle
    { hGetCategory =
        \catId -> findCategory catId . storageCategories <$> readIORef ref
    , hSetCategoryIdToNewsVersionsInCategoryAndDescendantCategories =
        \newCatId oldCatId -> do
          modifyIORef' ref $ \old@Storage {..} ->
            let descendantsOfOldCat = findDescendants oldCatId storageCategories
             in old
                  { storageRequestedDraftCategoryChanges =
                      foldl'
                        (flip Set.insert)
                        storageRequestedDraftCategoryChanges $
                      map (, newCatId) descendantsOfOldCat
                  , storageDrafts =
                      mapIf
                        ((`elem` map Just descendantsOfOldCat) . draftCategory)
                        (const $ StubDraft newCatId)
                        storageDrafts
                  }
    , hDeleteCategoryAndDescendants =
        \catId ->
          modifyIORef' ref $ \old@Storage {..} ->
            let descendants = findDescendants catId storageCategories
             in old
                  { storageRequestedCategoryDeletions =
                      foldl'
                        (flip Set.insert)
                        storageRequestedCategoryDeletions
                        descendants
                  , storageCategories =
                      filter ((`notElem` descendants) . fst) storageCategories
                  }
    }

findCategory :: CategoryId -> CategoryMap -> Maybe Category
findCategory categoryId cats =
  lookup categoryId cats <&> \optParentId ->
    Category
      { categoryId
      , categoryName = ""
      , categoryParent = (`findCategory` cats) =<< optParentId
      }

findDescendants :: CategoryId -> CategoryMap -> [CategoryId]
findDescendants ancestorId cats =
  case findCategory ancestorId cats of
    Nothing -> []
    Just _ -> go ancestorId
  where
    go parentId = parentId : concatMap go (findChildren parentId cats)

findChildren :: CategoryId -> CategoryMap -> [CategoryId]
findChildren parentId = map fst . filter ((Just parentId ==) . snd)

mapIf :: (a -> Bool) -> (a -> a) -> [a] -> [a]
mapIf test f =
  map $ \x ->
    if test x
      then f x
      else x
