module Core.Interactor.UpdateTagSpec
  ( spec
  ) where

import Control.Arrow
import Core.Authentication.Test
import Core.Exception
import Core.Interactor.UpdateTag
import Core.Tag
import qualified Data.HashMap.Strict as Map
import qualified Data.HashSet as Set
import Data.IORef
import Data.IORef.Util
import Data.List
import Test.Hspec

spec :: Spec
spec
  {- HLINT ignore spec "Reduce duplication" -}
 =
  describe "run" $ do
    it "should throw NoPermissionException if the user is not an admin" $ do
      let tagId = TagId 1
          initialData = storageWith [Tag {tagId = tagId, tagName = ""}]
      db <- newIORef initialData
      run (handleWith db) someNonAdminUser tagId "q" `shouldThrow`
        isNoPermissionException
      readIORef db `shouldReturn` initialData
    it
      "should return Left UnknownTagId if no such tag found, the name is not empty and the user is an admin" $ do
      let unknownTagId = TagId 1
          initialData = storageWith [Tag {tagId = TagId 2, tagName = ""}]
      db <- newIORef initialData
      r <- run (handleWith db) someAdminUser unknownTagId "q"
      r `shouldBe` Left UnknownTagId
      finalData <- readIORef db
      storageTags finalData `shouldBe` storageTags initialData
      [Set.singleton unknownTagId, Set.empty] `shouldContain`
        [storageRequestedTagChanges finalData]
    it
      "should return Left TagNameMustNotBeEmpty if the name is empty and the user is admin" $ do
      let tagId = TagId 1
          initialData = storageWith [Tag {tagId = tagId, tagName = ""}]
      db <- newIORef initialData
      r <- run (handleWith db) someAdminUser tagId ""
      r `shouldBe` Left TagNameMustNotBeEmpty
      readIORef db `shouldReturn` initialData
    it
      "should return Left TagNameMustBeUnique for existing tag if the name is non-empty, non-unique, and the user is admin" $ do
      let tagId = TagId 1
          existingTagName = "q"
          initialData =
            storageWith
              [ Tag {tagId, tagName = ""}
              , Tag {tagId = TagId 2, tagName = existingTagName}
              ]
      db <- newIORef initialData
      r <- run (handleWith db) someAdminUser tagId existingTagName
      r `shouldBe` Left TagNameMustBeUnique
      readIORef db `shouldReturn` initialData
    it
      "should return Right <updated tag> for existing tag if the name is non-empty, unique, and the user is admin" $ do
      let newName = "z"
          tagId = TagId 1
          initialData = storageWith [Tag {tagId, tagName = "a"}]
          expectedModifiedTag = Tag {tagId, tagName = newName}
      db <- newIORef initialData
      r <- run (handleWith db) someAdminUser tagId newName
      r `shouldBe` Right expectedModifiedTag
      finalData <- readIORef db
      storageTags finalData `shouldBe` storageTagsWith [expectedModifiedTag]
      [Set.singleton tagId, Set.empty] `shouldContain`
        [storageRequestedTagChanges finalData]
    it
      "should return Right <updated tag> for existing tag if the name is non-empty, matches the old name, and the user is admin" $ do
      let oldName = "a"
          tagId = TagId 1
          tag = Tag {tagId, tagName = oldName}
          initialData = storageWith [tag]
      db <- newIORef initialData
      r <- run (handleWith db) someAdminUser tagId oldName
      r `shouldBe` Right tag
      finalData <- readIORef db
      storageTags finalData `shouldBe` storageTags initialData
      [Set.singleton tagId, Set.empty] `shouldContain`
        [storageRequestedTagChanges finalData]

data Storage =
  Storage
    { storageTags :: Map.HashMap TagId Tag
    , storageRequestedTagChanges :: Set.HashSet TagId
    }
  deriving (Eq, Show)

storageWith :: [Tag] -> Storage
storageWith tags =
  Storage
    {storageTags = storageTagsWith tags, storageRequestedTagChanges = Set.empty}

storageTagsWith :: [Tag] -> Map.HashMap TagId Tag
storageTagsWith = Map.fromList . map (tagId &&& id)

handleWith :: IORef Storage -> Handle IO
handleWith ref =
  Handle
    { hFindTagNamed =
        \name ->
          fmap fst . find ((name ==) . tagName . snd) . Map.toList . storageTags <$>
          readIORef ref
    , hSetTagName =
        \tagId newName ->
          updateIORef' ref $ \Storage {..} ->
            ( Storage
                { storageTags =
                    Map.adjust
                      (\tag -> tag {tagName = newName})
                      tagId
                      storageTags
                , storageRequestedTagChanges =
                    Set.insert tagId storageRequestedTagChanges
                }
            , if tagId `Map.member` storageTags
                then Right ()
                else Left STNUnknownTagId)
    }
