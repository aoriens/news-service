{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | The module describes a path-driven routing machinery of the web server.
module Web.Router
  ( Router
  , new
  , method
  , get
  , post
  , put
  , delete
  , patch
  , route
  , Result(..)
  , isHandlerResult
  , isResourceNotFoundResult
  , isMethodNotSupportedResult
  ) where

import Control.Monad.Writer.Strict
import qualified Data.HashMap.Strict as HM
import Data.List hiding (delete)
import qualified Network.HTTP.Types as Http
import qualified Network.Wai as Wai
import qualified Web.AppURI as U
import Web.Types

-- | The router type is responsible for finding handlers for the given
-- URI paths and HTTP methods and for handling some exceptional cases.
newtype Router =
  Router (U.AppURI -> MethodsToHandlers)

type MethodsToHandlers = HM.HashMap Http.Method EApplication

-- | A monad type to make it easier to specify HTTP method to handler
-- mappings.
newtype MethodsSpec a =
  MethodsSpec (Writer MethodsToHandlersMonoid a)
  deriving (Functor, Applicative, Monad)

newtype MethodsToHandlersMonoid =
  MethodsToHandlersMonoid MethodsToHandlers

instance Semigroup MethodsToHandlersMonoid where
  (MethodsToHandlersMonoid hm1) <> (MethodsToHandlersMonoid hm2) =
    MethodsToHandlersMonoid $
    HM.unionWithKey
      (\path_ _ _ -> error $ "Duplicate entry for path " ++ show path_)
      hm1
      hm2

instance Monoid MethodsToHandlersMonoid where
  mempty = MethodsToHandlersMonoid mempty

execMethodsSpec :: MethodsSpec () -> MethodsToHandlers
execMethodsSpec (MethodsSpec w) =
  let (MethodsToHandlersMonoid table) = execWriter w
   in table

{- |

Creates a router. It is possible to use 'MethodSpec' monad to
configure HTTP method selection.

> new $ \uri -> case uri of
>   UserURI userId -> do
>     get    handleGetForUserId userId
>     put    handlePutForUserId userId
>   UsersURI -> do
>     post handlePostForUser

If the found entry does not contain a subentry for the request method,
it is reported as 'MethodNotSupportedResult'.

-}
new :: (U.AppURI -> MethodsSpec ()) -> Router
new f = Router $ execMethodsSpec . f

-- | Sets a handler for the specified HTTP method.
method :: Http.Method -> EApplication -> MethodsSpec ()
method m handler =
  MethodsSpec . tell . MethodsToHandlersMonoid $ HM.singleton m handler

get, post, put, delete, patch :: EApplication -> MethodsSpec ()
get = method Http.methodGet

post = method Http.methodPost

put = method Http.methodPut

delete = method Http.methodDelete

patch = method Http.methodPatch

-- | The result of finding a route.
data Result
  -- | A handler is found for the specified request.
  = HandlerResult EApplication
  -- | No handler is found. This is typically output as HTTP 404.
  | ResourceNotFoundResult
  -- | The requested resource does not support a specified method. The
  -- parameter contains known, sorted methods. This is typically
  -- output as HTTP 405 with Allow header.
  | MethodNotSupportedResult [Http.Method]

-- | Find a handler for the specified request.
route :: Router -> Wai.Request -> Result
route r request =
  case lookupMethodTable r request of
    Nothing -> ResourceNotFoundResult
    Just methodTable ->
      case HM.lookup (Wai.requestMethod request) methodTable of
        Nothing -> MethodNotSupportedResult (sort (HM.keys methodTable))
        Just handler -> HandlerResult handler

lookupMethodTable :: Router -> Wai.Request -> Maybe MethodsToHandlers
lookupMethodTable (Router handler) request =
  handler <$> U.fromRelativeURI (U.RelativeURI $ Wai.pathInfo request)

isHandlerResult :: Result -> Bool
isHandlerResult (HandlerResult _) = True
isHandlerResult _ = False

isResourceNotFoundResult :: Result -> Bool
isResourceNotFoundResult ResourceNotFoundResult = True
isResourceNotFoundResult _ = False

isMethodNotSupportedResult :: Result -> Bool
isMethodNotSupportedResult (MethodNotSupportedResult _) = True
isMethodNotSupportedResult _ = False

instance Show Result where
  show r =
    case r of
      HandlerResult _ -> "HandlerResult <handler>"
      ResourceNotFoundResult -> "ResourceNotFoundResult"
      MethodNotSupportedResult methods ->
        showString "MethodNotSupportedResult " . showParen True (shows methods) $
        ""
