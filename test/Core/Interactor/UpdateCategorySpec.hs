module Core.Interactor.UpdateCategorySpec
  ( spec
  ) where

import Control.Arrow
import Control.Monad
import Core.Authentication.Test
import Core.Category
import Core.Exception
import Core.Interactor.UpdateCategory
import Core.Stubs
import Data.Functor
import qualified Data.HashMap.Strict as Map
import qualified Data.HashSet as Set
import Data.IORef
import Data.IORef.Util
import Data.List
import qualified Data.List.NonEmpty as NonEmpty
import Data.List.NonEmpty (NonEmpty(..))
import Data.Maybe
import qualified Data.Text as T
import Test.Hspec

spec :: Spec
spec
  {- HLINT ignore spec "Reduce duplication" -}
 =
  describe "run" $ do
    it "should throw NoPermissionException if the user is not an admin" $ do
      let catId = CategoryId 1
          initialData = storageWith [stubCategory {categoryId = catId}]
          request = emptyRequest catId
      db <- newIORef initialData
      run (handleWith db) someNonAdminUser request `shouldThrow`
        isNoPermissionException
      readIORef db `shouldReturn` initialData
    it
      "should return NewNameMustNoBeEmpty if the new name is Just \"\" and the \
      \user is not an admin" $ do
      let catId = CategoryId 1
          initialData = storageWith [stubCategory {categoryId = catId}]
          request = (emptyRequest catId) {rNewName = Just ""}
      db <- newIORef initialData
      r <- run (handleWith db) someAdminUser request
      r `shouldBe` Left NameMustNotBeEmpty
      readIORef db `shouldReturn` initialData
    it
      "should return the unchanged category if it exists, request fields are \
      \Nothing, and the user is admin" $ do
      let catId = CategoryId 1
          cat = stubCategory {categoryId = catId}
          initialData = storageWith [cat]
          request = emptyRequest catId
      db <- newIORef initialData
      r <- run (handleWith db) someAdminUser request
      r `shouldBe` Right cat
      readIORef db `shouldReturn` initialData
    it
      "should return Left UnknownCategoryId if the category does not exist, and \
      \the user is admin" $ do
      let unknownCatId = CategoryId 1
          initialData = storageWith [stubCategory {categoryId = CategoryId 2}]
          request = emptyRequest unknownCatId
      db <- newIORef initialData
      r <- run (handleWith db) someAdminUser request
      r `shouldBe` Left UnknownCategoryId
      readIORef db `shouldReturn` initialData
    it
      "should update an existing category, if the new name is non-empty, unique \
      \among siblings, and the user is admin" $ do
      let catId = CategoryId 1
          oldCat = stubCategory {categoryId = catId, categoryName = "1"}
          newName = "2"
          initialData = storageWith [oldCat]
          request = (emptyRequest catId) {rNewName = Just newName}
          newCat = oldCat {categoryName = newName}
      db <- newIORef initialData
      r <- run (handleWith db) someAdminUser request
      r `shouldBe` Right newCat
      finalData <- readIORef db
      storageCategories finalData `shouldBe` storageCategoryMap [newCat]
      updatedCategoriesOtherThan catId finalData `shouldBe` []
    it
      "should return Left NameMustBeUniqueAmongSiblings if the category exists, \
      \its name is not unique among siblings, and the user is admin" $ do
      let catId = CategoryId 1
          newName = "2"
          initialData =
            storageWith
              [ stubCategory {categoryId = catId, categoryName = "1"}
              , stubCategory {categoryId = CategoryId 2, categoryName = newName}
              ]
          request = (emptyRequest catId) {rNewName = Just newName}
      db <- newIORef initialData
      r <- run (handleWith db) someAdminUser request
      r `shouldBe` Left NameMustBeUniqueAmongSiblings
      readIORef db `shouldReturn` initialData
    it
      "should return an existing category unchanged if the new name is \
      \unchanged, and the user is admin" $ do
      let catId = CategoryId 1
          oldName = "2"
          oldCat = stubCategory {categoryId = catId, categoryName = oldName}
          initialData = storageWith [oldCat]
          request = (emptyRequest catId) {rNewName = Just oldName}
      db <- newIORef initialData
      r <- run (handleWith db) someAdminUser request
      r `shouldBe` Right oldCat
      finalData <- readIORef db
      storageCategories finalData `shouldBe` storageCategoryMap [oldCat]
      updatedCategoriesOtherThan catId finalData `shouldBe` []
    it
      "should update an existing category if the new parent exists, no loops, \
      \and the user is admin" $ do
      let catId = CategoryId 1
          oldCat =
            stubCategory
              {categoryId = catId, categoryName = "1", categoryParent = Nothing}
          newParentId = CategoryId 2
          newParent =
            stubCategory {categoryId = newParentId, categoryName = "2"}
          oldChild =
            stubCategory
              {categoryId = CategoryId 3, categoryParent = Just oldCat}
          newChild = oldChild {categoryParent = Just newCat}
          initialData = storageWith [oldCat, newParent, oldChild]
          request = (emptyRequest catId) {rNewParent = Just $ Just newParentId}
          newCat = oldCat {categoryParent = Just newParent}
      db <- newIORef initialData
      r <- run (handleWith db) someAdminUser request
      r `shouldBe` Right newCat
      finalData <- readIORef db
      storageCategories finalData `shouldBe`
        storageCategoryMap [newCat, newChild]
      updatedCategoriesOtherThan catId finalData `shouldBe` []
    it
      "should update an existing non-root category if the new parent is Nothing \
      \(root), no loops, and the user is admin" $ do
      let catId = CategoryId 1
          oldCat =
            stubCategory
              { categoryId = catId
              , categoryName = "1"
              , categoryParent = Just oldParent
              }
          oldChild =
            stubCategory
              {categoryId = CategoryId 3, categoryParent = Just oldCat}
          oldParent =
            stubCategory
              {categoryId = CategoryId 4, categoryName = "old parent"}
          newChild = oldChild {categoryParent = Just newCat}
          initialData = storageWith [oldCat, oldParent, oldChild]
          newCat = oldCat {categoryParent = Nothing}
          request = (emptyRequest catId) {rNewParent = Just Nothing}
      db <- newIORef initialData
      r <- run (handleWith db) someAdminUser request
      r `shouldBe` Right newCat
      finalData <- readIORef db
      storageCategories finalData `shouldBe`
        storageCategoryMap [newCat, newChild, oldParent]
      updatedCategoriesOtherThan catId finalData `shouldBe` []
    it
      "should return Left UnknownNewParentId if a category exists, but the new \
      \parent does not, and the user is admin" $ do
      let catId = CategoryId 1
          oldCat =
            stubCategory
              {categoryId = catId, categoryName = "1", categoryParent = Nothing}
          newParentId = CategoryId 2
          initialData = storageWith [oldCat]
          request = (emptyRequest catId) {rNewParent = Just $ Just newParentId}
      db <- newIORef initialData
      r <- run (handleWith db) someAdminUser request
      r `shouldBe` Left UnknownNewParentId
      readIORef db `shouldReturn` initialData
    it
      "should return Left AncestryLoopDetected if a category exists, but the \
      \new parent is its descendant, and the user is admin" $ do
      let catId = CategoryId 1
          cat =
            stubCategory
              {categoryId = catId, categoryName = "1", categoryParent = Nothing}
          child =
            stubCategory
              { categoryId = CategoryId 2
              , categoryName = "2"
              , categoryParent = Just cat
              }
          grandchildId = CategoryId 3
          grandchild =
            stubCategory
              { categoryId = grandchildId
              , categoryName = "3"
              , categoryParent = Just child
              }
          initialData = storageWith [cat, child, grandchild]
          request = (emptyRequest catId) {rNewParent = Just $ Just grandchildId}
      db <- newIORef initialData
      r <- run (handleWith db) someAdminUser request
      r `shouldBe` Left AncestryLoopDetected
      readIORef db `shouldReturn` initialData
    it
      "should return Left AncestryLoopDetected if an existing category is moved to \
      \itself, and the user is admin" $ do
      let catId = CategoryId 1
          cat =
            stubCategory
              {categoryId = catId, categoryName = "1", categoryParent = Nothing}
          initialData = storageWith [cat]
          request = (emptyRequest catId) {rNewParent = Just $ Just catId}
      db <- newIORef initialData
      r <- run (handleWith db) someAdminUser request
      r `shouldBe` Left AncestryLoopDetected
      readIORef db `shouldReturn` initialData
    it
      "should update an existing category if the new parent equals to the old \
      \one, and the user is admin" $ do
      let catId = CategoryId 1
          parentId = CategoryId 2
          cat =
            stubCategory
              { categoryId = catId
              , categoryName = "1"
              , categoryParent = Just parent
              }
          parent = stubCategory {categoryId = parentId, categoryName = "2"}
          initialData = storageWith [cat]
          request = (emptyRequest catId) {rNewParent = Just $ Just parentId}
      db <- newIORef initialData
      r <- run (handleWith db) someAdminUser request
      r `shouldBe` Right cat
      finalData <- readIORef db
      storageCategories finalData `shouldBe` storageCategoryMap [cat]
      updatedCategoriesOtherThan catId finalData `shouldBe` []
    it
      "should return Left NameMustBeUniqueAmongSiblings if a category is moved \
      \into another parent, the name is unchanged and is not unique among new \
      \siblings, and the user is admin" $ do
      let catId = CategoryId 1
          oldName = "old name"
          cat =
            stubCategory
              { categoryId = catId
              , categoryParent = Just oldParent
              , categoryName = oldName
              }
          oldParent =
            stubCategory
              {categoryId = CategoryId 2, categoryName = "old parent"}
          newParentId = CategoryId 10
          newParent =
            stubCategory {categoryId = newParentId, categoryName = "new parent"}
          newSibling =
            stubCategory
              { categoryId = CategoryId 11
              , categoryParent = Just newParent
              , categoryName = oldName
              }
          initialData = storageWith [cat, oldParent, newParent, newSibling]
          request = (emptyRequest catId) {rNewParent = Just $ Just newParentId}
      db <- newIORef initialData
      r <- run (handleWith db) someAdminUser request
      r `shouldBe` Left NameMustBeUniqueAmongSiblings
      readIORef db `shouldReturn` initialData
    it
      "should return Left NameMustBeUniqueAmongSiblings if a category is moved \
      \into another parent, the new name is not unique among new siblings, and \
      \the user is admin" $ do
      let catId = CategoryId 1
          newName = "new name"
          cat =
            stubCategory
              { categoryId = catId
              , categoryParent = Just oldParent
              , categoryName = "old name"
              }
          oldParent =
            stubCategory
              {categoryId = CategoryId 2, categoryName = "old parent"}
          newParentId = CategoryId 10
          newParent =
            stubCategory {categoryId = newParentId, categoryName = "new parent"}
          newSibling =
            stubCategory
              { categoryId = CategoryId 11
              , categoryParent = Just newParent
              , categoryName = newName
              }
          initialData = storageWith [cat, oldParent, newParent, newSibling]
          request =
            (emptyRequest catId)
              {rNewParent = Just $ Just newParentId, rNewName = Just newName}
      db <- newIORef initialData
      r <- run (handleWith db) someAdminUser request
      r `shouldBe` Left NameMustBeUniqueAmongSiblings
      readIORef db `shouldReturn` initialData
    it
      "should update an existing category with the new parent and name, if \
      \no loops created, the new name is unique among new siblings, and the \
      \user is admin" $ do
      let catId = CategoryId 1
          newName = "new name"
          newParentId = CategoryId 2
          oldCat =
            stubCategory
              { categoryId = catId
              , categoryParent = Just oldParent
              , categoryName = "old name"
              }
          newCat =
            oldCat {categoryParent = Just newParent, categoryName = newName}
          oldParent =
            stubCategory
              {categoryId = CategoryId 3, categoryName = "old parent"}
          newParent =
            stubCategory {categoryId = newParentId, categoryName = "new parent"}
          initialData = storageWith [oldCat, oldParent, newParent]
          request =
            (emptyRequest catId)
              {rNewParent = Just $ Just newParentId, rNewName = Just newName}
      db <- newIORef initialData
      r <- run (handleWith db) someAdminUser request
      r `shouldBe` Right newCat
      finalData <- readIORef db
      storageCategories finalData `shouldBe`
        storageCategoryMap [newCat, oldParent, newParent]
      updatedCategoriesOtherThan catId finalData `shouldBe` []
    it
      "should update an existing category with the new parent and name, if \
      \no loops created, the new name is unique among new siblings, the \
      \user is admin, the old name might conflict with new siblings, and the \
      \new name might conflict with old siblings" $ do
      let catId = CategoryId 1
          newParentId = CategoryId 2
          oldCat =
            stubCategory
              { categoryId = catId
              , categoryParent = Just oldParent
              , categoryName = "old name"
              }
          newCat =
            oldCat {categoryParent = Just newParent, categoryName = "new name"}
          oldParent =
            stubCategory
              {categoryId = CategoryId 3, categoryName = "old parent"}
          newParent =
            stubCategory {categoryId = newParentId, categoryName = "new parent"}
          oldSibling =
            stubCategory
              { categoryId = CategoryId 4
              , categoryName = "new name"
              , categoryParent = Just oldParent
              }
          newSibling =
            stubCategory
              { categoryId = CategoryId 5
              , categoryName = "old name"
              , categoryParent = Just newParent
              }
          initialData =
            storageWith [oldCat, oldParent, oldSibling, newParent, newSibling]
          request =
            (emptyRequest catId)
              {rNewParent = Just $ Just newParentId, rNewName = Just "new name"}
      db <- newIORef initialData
      r <- run (handleWith db) someAdminUser request
      r `shouldBe` Right newCat
      finalData <- readIORef db
      storageCategories finalData `shouldBe`
        storageCategoryMap
          [newCat, oldParent, oldSibling, newParent, newSibling]
      updatedCategoriesOtherThan catId finalData `shouldBe` []

data Storage =
  Storage
    { storageCategories :: Map.HashMap CategoryId StoredCategory
    , storageRequestedCategoryUpdates :: Set.HashSet CategoryId
    }
  deriving (Show, Eq)

data StoredCategory =
  StoredCategory
    { scId :: CategoryId
    , scParentId :: Maybe CategoryId
    , scName :: T.Text
    }
  deriving (Eq, Show)

storageWith :: [Category] -> Storage
storageWith cats =
  Storage
    { storageCategories = storageCategoryMap cats
    , storageRequestedCategoryUpdates = Set.empty
    }

storageCategoryMap :: [Category] -> Map.HashMap CategoryId StoredCategory
storageCategoryMap =
  Map.fromList .
  map (categoryId &&& toStoredCategory) .
  concatMap (NonEmpty.toList . categoryAncestry)

toStoredCategory :: Category -> StoredCategory
toStoredCategory cat =
  StoredCategory
    { scId = categoryId cat
    , scParentId = categoryId <$> categoryParent cat
    , scName = categoryName cat
    }

retrieveCategory ::
     Map.HashMap CategoryId StoredCategory -> CategoryId -> Maybe Category
retrieveCategory smap catId =
  Map.lookup catId smap <&> \cat0 ->
    if hasAncestryLoop cat0
      then error "Broken ancestry in the storage: loop detected"
      else unwind cat0
  where
    unwind StoredCategory {..} =
      Category
        { categoryId = scId
        , categoryName = scName
        , categoryParent =
            unwind .
            fromMaybe
              (error "Broken ancestry in the storage: unknown categoryId") .
            (`Map.lookup` smap) <$>
            scParentId
        }
    hasAncestryLoop scat = Just () == findLoop scat scat
    findLoop slow fast = do
      slow' <- (`Map.lookup` smap) =<< scParentId slow
      fast' <-
        (`Map.lookup` smap) =<<
        scParentId =<< (`Map.lookup` smap) =<< scParentId fast
      if slow' == fast'
        then Just ()
        else findLoop slow' fast'

emptyRequest :: CategoryId -> Request
emptyRequest rCategoryId =
  Request {rCategoryId, rNewName = Nothing, rNewParent = Nothing}

updatedCategoriesOtherThan :: CategoryId -> Storage -> [CategoryId]
updatedCategoriesOtherThan optCatId =
  filter (optCatId /=) . Set.toList . storageRequestedCategoryUpdates

handleWith :: IORef Storage -> Handle IO
handleWith ref =
  Handle
    { hUpdateCategory = updateIORef' ref . updateCategoryInStorage
    , hGetCategoryIdBySiblingAndName =
        \siblingId name -> do
          catsMap <- storageCategories <$> readIORef ref
          let parentId = scParentId . fromJust $ Map.lookup siblingId catsMap
              test cat = scName cat == name && scParentId cat == parentId
          pure . fmap scId . find test $ Map.elems catsMap
    , hGetCategoryIdByParentAndName =
        \optParentId name -> do
          catsMap <- storageCategories <$> readIORef ref
          let test cat = scName cat == name && scParentId cat == optParentId
          pure . fmap scId . find test $ Map.elems catsMap
    , hCategoryIsDescendantOf =
        \descendantId ancestorId ->
          categoryIsDescendantOf descendantId ancestorId . storageCategories <$>
          readIORef ref
    , hGetCategoryName =
        \catId ->
          fmap scName . Map.lookup catId . storageCategories <$> readIORef ref
    }

updateCategoryInStorage ::
     Request -> Storage -> (Storage, Either UpdateCategoryFailure Category)
updateCategoryInStorage r oldStorage@Storage {..} =
  case rNewParent r of
    Just (Just newParentId)
      | not $ newParentId `Map.member` storageCategories ->
        (oldStorage, Left UCUnknownNewParentId)
    _ -> (newStorage, result)
  where
    newStorage =
      Storage
        { storageCategories = storageCategories'
        , storageRequestedCategoryUpdates =
            (if requestIsNoOp r
               then id
               else Set.insert (rCategoryId r))
              storageRequestedCategoryUpdates
        }
    result =
      maybe (Left UCUnknownCategoryId) Right $
      retrieveCategory storageCategories' $ rCategoryId r
    storageCategories' =
      Map.adjust (updateStoredCategory r) (rCategoryId r) storageCategories

requestIsNoOp :: Request -> Bool
requestIsNoOp Request {..} = isNothing rNewName && isNothing rNewParent

updateStoredCategory :: Request -> StoredCategory -> StoredCategory
updateStoredCategory Request {..} cat =
  cat
    { scName = fromMaybe (scName cat) rNewName
    , scParentId = fromMaybe (scParentId cat) rNewParent
    }

categoryIsDescendantOf ::
     CategoryId -> CategoryId -> Map.HashMap CategoryId StoredCategory -> Bool
categoryIsDescendantOf descendantId ancestorId smap =
  elem ancestorId $ storedCategoryAncestryIds smap descendantId

storedCategoryAncestryIds ::
     Map.HashMap CategoryId StoredCategory -> CategoryId -> NonEmpty CategoryId
storedCategoryAncestryIds smap =
  NonEmpty.unfoldr $ id &&& (scParentId <=< (`Map.lookup` smap))
{-

emptyNameCategoryExists :: Storage -> Bool
emptyNameCategoryExists =
  any (T.null . categoryName) . Map.elems . storageCategories

siblingCategoriesWithEqualNamesExist :: Storage -> Bool
siblingCategoriesWithEqualNamesExist Storage {..} =
  any hasEqualNames . NonEmpty.groupBy ((==) `on` categoryParent) $
  sortBy compareCategoriesByParentAndName $ Map.elems storageCategories
  where
    hasEqualNames (c :| cs) = or $ zipWith ((==) `on` categoryName) (c : cs) cs

compareCategoriesByParentAndName :: Category -> Category -> Ordering
compareCategoriesByParentAndName =
  comparing (fmap (getCategoryId . categoryId) . categoryParent) <>
  comparing categoryName

findCategoryWithAncestryLoop :: Storage -> Maybe CategoryId
findCategoryWithAncestryLoop =
  fmap fst .
  find (categoryHasAncestryLoop . snd) . Map.toList . storageCategories

categoryHasAncestryLoop :: Category -> Bool
categoryHasAncestryLoop cat = Just () == go cat cat
  where
    -- The Floyd's algorithm uses two pointers: the first forwards by
    -- one, and the other does by two. If they meet at any time
    -- (except the start node), there is a loop.
    go slow fast = do
      slow' <- categoryParent slow
      fast' <- categoryParent =<< categoryParent fast
      if slow' == fast'
        then Just ()
        else go slow' fast'
        
-}
