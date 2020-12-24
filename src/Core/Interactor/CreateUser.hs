{-# LANGUAGE RankNTypes #-}

module Core.Interactor.CreateUser
  ( run
  , Handle(..)
  , Request(..)
  , CreateUserCommand(..)
  , CreateUserResult(..)
  ) where

import Control.Monad.Catch
import qualified Core.Authentication as Auth
import Core.Image
import Core.User
import Data.Text (Text)
import Data.Time.Clock

data Handle m =
  Handle
    { hCreateUser :: CreateUserCommand -> m CreateUserResult
    , hGenerateToken :: m (Auth.SecretToken, Auth.SecretTokenHash)
    , hGetCurrentTime :: m UTCTime
    , hRejectImageIfDisallowed :: MonadThrow m =>
                                    Image -> m ()
    }

-- | Run the interactor. It can throw 'IncorrectParameterException'
run :: MonadThrow m => Handle m -> Request -> m (User, Auth.Credentials)
run Handle {..} Request {..} = do
  let isAdmin = False
  mapM_ hRejectImageIfDisallowed rAvatar
  (token, tokenHash) <- hGenerateToken
  createdAt <- hGetCurrentTime
  result <-
    hCreateUser
      CreateUserCommand
        { cuFirstName = rFirstName
        , cuLastName = rLastName
        , cuAvatar = rAvatar
        , cuCreatedAt = createdAt
        , cuIsAdmin = isAdmin
        , cuTokenHash = tokenHash
        }
  pure
    ( User
        { userId = curUserId result
        , userFirstName = rFirstName
        , userLastName = rLastName
        , userAvatarId = curAvatarId result
        , userCreatedAt = createdAt
        , userIsAdmin = isAdmin
        }
    , Auth.TokenCredentials (curUserId result) token)

data Request =
  Request
    { rFirstName :: Maybe Text
      -- ^ The first name. This is unnecessary in case of a
      -- single-component name.
    , rLastName :: Text
    , rAvatar :: Maybe Image
    }

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
