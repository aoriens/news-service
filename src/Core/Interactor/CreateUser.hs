{-# LANGUAGE RecordWildCards #-}

module Core.Interactor.CreateUser
  ( run
  , Handle(..)
  , Query(..)
  , ImageQuery
  , CreateUserCommand(..)
  , CreateUserResult(..)
  ) where

import Control.Monad
import Control.Monad.Catch
import qualified Core.Authentication as Auth
import Core.DTO.Image
import Core.DTO.User
import Core.Exception
import qualified Data.HashSet as HS
import Data.List
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock

data Handle m =
  Handle
    { hCreateUser :: CreateUserCommand -> m CreateUserResult
    , hGenerateToken :: m (Auth.SecretToken, Auth.SecretTokenHash)
    , hGetCurrentTime :: m UTCTime
    , hAllowedImageContentTypes :: HS.HashSet Text
    }

-- | Run the interactor. It can throw 'QueryException'
run :: MonadThrow m => Handle m -> Query -> m (User, Auth.SecretToken)
run h@Handle {..} q@Query {..} = do
  let isAdmin = False
  rejectDisallowedAvatarContentType h q
  (token, tokenHash) <- hGenerateToken
  createdAt <- hGetCurrentTime
  result <-
    hCreateUser
      CreateUserCommand
        { cuFirstName = qFirstName
        , cuLastName = qLastName
        , cuAvatar = qAvatar
        , cuCreatedAt = createdAt
        , cuIsAdmin = isAdmin
        , cuTokenHash = tokenHash
        }
  pure
    ( User
        { userId = curUserId result
        , userFirstName = qFirstName
        , userLastName = qLastName
        , userAvatarId = curAvatarId result
        , userCreatedAt = createdAt
        , userIsAdmin = isAdmin
        }
    , token)

rejectDisallowedAvatarContentType :: MonadThrow m => Handle m -> Query -> m ()
rejectDisallowedAvatarContentType Handle {..} Query {..} =
  case qAvatar of
    Just Image {..} ->
      when (imageContentType `notElem` hAllowedImageContentTypes) $
      throwM
        (disallowedAvatarContentTypeException
           imageContentType
           hAllowedImageContentTypes)
    Nothing -> pure ()

data Query =
  Query
    { qFirstName :: Maybe Text
      -- ^ The first name. This is unnecessary in case of a
      -- single-component name.
    , qLastName :: Text
    , qAvatar :: Maybe ImageQuery
    }

type ImageQuery = Image

data CreateUserCommand =
  CreateUserCommand
    { cuFirstName :: Maybe Text
    , cuLastName :: Text
    , cuAvatar :: Maybe Image
    , cuCreatedAt :: UTCTime
    , cuIsAdmin :: Bool
    , cuTokenHash :: Auth.SecretTokenHash
    }

data CreateUserResult =
  CreateUserResult
    { curUserId :: UserId
    , curAvatarId :: Maybe ImageId
    }

disallowedAvatarContentTypeException ::
     Text -> HS.HashSet Text -> QueryException
disallowedAvatarContentTypeException badContentType allowedContentTypes =
  QueryException $
  mconcat
    [ "Content type '"
    , badContentType
    , "' is disallowed. Allowed content types: "
    ] <>
  T.intercalate ", " (sort $ HS.toList allowedContentTypes)
