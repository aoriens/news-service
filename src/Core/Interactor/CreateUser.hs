module Core.Interactor.CreateUser
  ( run
  , Handle(..)
  , Query(..)
  , ImageQuery
  , CreateUserCommand(..)
  , CreateUserResult(..)
  ) where

import Control.Monad.Catch
import qualified Core.Authentication as Auth
import Core.Image
import Core.ImageValidator
import Core.User
import qualified Data.HashSet as HS
import Data.Text (Text)
import Data.Time.Clock

data Handle m =
  Handle
    { hCreateUser :: CreateUserCommand -> m CreateUserResult
    , hGenerateToken :: m (Auth.SecretToken, Auth.SecretTokenHash)
    , hGetCurrentTime :: m UTCTime
    , hAllowedImageContentTypes :: HS.HashSet Text
    }

-- | Run the interactor. It can throw 'QueryException'
run :: MonadThrow m => Handle m -> Query -> m (User, Auth.Credentials)
run Handle {..} Query {..} = do
  let isAdmin = False
  mapM_ (rejectDisallowedImage hAllowedImageContentTypes) qAvatar
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
    , Auth.TokenCredentials (curUserId result) token)

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
