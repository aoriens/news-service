module Core.Interactor.UpdateCategory
  ( run
  , Handle(..)
  , Request(..)
  , Failure(..)
  , UpdateCategoryFailure(..)
  ) where

import Control.Monad
import Control.Monad.Catch
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Core.Authorization
import Core.Category
import Data.Maybe.Util
import qualified Data.Text as T

data Handle m =
  Handle
    { hUpdateCategory :: Request -> m (Either UpdateCategoryFailure Category)
    , hGetCategoryIdBySiblingAndName :: CategoryId -> T.Text -> m (Maybe CategoryId)
    , hGetCategoryIdByParentAndName :: Maybe CategoryId -> T.Text -> m (Maybe CategoryId)
    , hCategoryIsDescendantOf :: CategoryId -> CategoryId -> m Bool
    -- ^ Parameters: descendant, ancestor
    , hGetCategoryName :: CategoryId -> m (Maybe T.Text)
    }

data UpdateCategoryFailure
  = UCUnknownCategoryId
  | UCUnknownNewParentId
  deriving (Eq, Show)

run ::
     MonadThrow m
  => Handle m
  -> AuthenticatedUser
  -> Request
  -> m (Either Failure Category)
run h authUser request@Request {..} =
  runExceptT $ do
    lift $ authorize "modify a category" $ authUserShouldBeAdmin authUser
    mapM_ newNameMustNotBeEmpty rNewName
    nameMustBeUniqueAmongNewSiblings h request
    mapM_ (changingParentMustNotMakeLoops h request) rNewParent
    lift (hUpdateCategory h request) >>= \case
      Right cat -> pure cat
      Left UCUnknownCategoryId -> throwE UnknownCategoryId
      Left UCUnknownNewParentId -> throwE UnknownNewParentId

-- | A request to update the category
data Request =
  Request
    { rCategoryId :: CategoryId
    , rNewName :: Maybe T.Text
    , rNewParent :: Maybe (Maybe CategoryId)
      -- ^ An optional new parent of the category. 'Nothing' means
      -- non-changing the parent, 'Just Nothing' means setting the parent
      -- to 'Nothing'.
    }

data Failure
  = UnknownCategoryId
  | UnknownNewParentId
  | NameMustNotBeEmpty
  | NameMustBeUniqueAmongSiblings
  | AncestryLoopDetected
  deriving (Eq, Show)

newNameMustNotBeEmpty :: Monad m => T.Text -> ExceptT Failure m ()
newNameMustNotBeEmpty newName = when (newName == "") $ throwE NameMustNotBeEmpty

nameMustBeUniqueAmongNewSiblings ::
     Monad m => Handle m -> Request -> ExceptT Failure m ()
nameMustBeUniqueAmongNewSiblings h Request {..} =
  ok >>= (`unless` throwE NameMustBeUniqueAmongSiblings)
  where
    ok = (`elem` [Nothing, Just rCategoryId]) <$> getConflictingCategoryId
    getConflictingCategoryId =
      case (rNewName, rNewParent) of
        (Just newName, Nothing) ->
          lift $ hGetCategoryIdBySiblingAndName h rCategoryId newName
        (Just newName, Just newParent) ->
          lift $ hGetCategoryIdByParentAndName h newParent newName
        (Nothing, Just newParent) -> do
          oldName <- getCategoryName h rCategoryId
          lift $ hGetCategoryIdByParentAndName h newParent oldName
        (Nothing, Nothing) -> pure Nothing

getCategoryName :: Monad m => Handle m -> CategoryId -> ExceptT Failure m T.Text
getCategoryName h catId =
  lift (hGetCategoryName h catId) >>= fromMaybeM (throwE UnknownCategoryId)

changingParentMustNotMakeLoops ::
     Monad m => Handle m -> Request -> Maybe CategoryId -> ExceptT Failure m ()
changingParentMustNotMakeLoops h Request {..} =
  (`unless` throwE AncestryLoopDetected) <=< isOk
  where
    isOk Nothing = pure True
    isOk (Just newParentId)
      | newParentId == rCategoryId = pure False
      | otherwise =
        not <$> lift (hCategoryIsDescendantOf h newParentId rCategoryId)
