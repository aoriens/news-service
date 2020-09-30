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

-- | The router type is responsible for finding handlers for the given
-- URI paths and HTTP methods and for handling some exceptional cases.
-- It is parameterized with the request handler type.
newtype Router handler =
  Router (U.AppURI -> MethodsToHandlers handler)

type MethodsToHandlers = HM.HashMap Http.Method

-- | A monad type to make it easier to specify HTTP method to handler
-- mappings.
newtype MethodsSpec handler a =
  MethodsSpec (Writer (MethodsToHandlersMonoid handler) a)
  deriving (Functor, Applicative, Monad)

newtype MethodsToHandlersMonoid handler =
  MethodsToHandlersMonoid (MethodsToHandlers handler)

instance Semigroup (MethodsToHandlersMonoid h) where
  (MethodsToHandlersMonoid hm1) <> (MethodsToHandlersMonoid hm2) =
    MethodsToHandlersMonoid $
    HM.unionWithKey
      (\path_ _ _ -> error $ "Duplicate entry for path " ++ show path_)
      hm1
      hm2

instance Monoid (MethodsToHandlersMonoid h) where
  mempty = MethodsToHandlersMonoid mempty

execMethodsSpec :: MethodsSpec handler () -> MethodsToHandlers handler
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
new :: (U.AppURI -> MethodsSpec handler ()) -> Router handler
new f = Router $ execMethodsSpec . f

-- | Sets a handler for the specified HTTP method.
method :: Http.Method -> handler -> MethodsSpec handler ()
method m handler =
  MethodsSpec . tell . MethodsToHandlersMonoid $ HM.singleton m handler

get, post, put, delete, patch :: handler -> MethodsSpec handler ()
get = method Http.methodGet

post = method Http.methodPost

put = method Http.methodPut

delete = method Http.methodDelete

patch = method Http.methodPatch

-- | The result of finding a route.
data Result handler
  -- | A handler is found for the specified request.
  = HandlerResult handler
  -- | No handler is found. This is typically output as HTTP 404.
  | ResourceNotFoundResult
  -- | The requested resource does not support a specified method. The
  -- parameter contains known, sorted methods. This is typically
  -- output as HTTP 405 with Allow header.
  | MethodNotSupportedResult [Http.Method]

-- | Find a handler for the specified request.
route :: Router handler -> Wai.Request -> Result handler
route r request =
  case lookupMethodTable r request of
    Nothing -> ResourceNotFoundResult
    Just methodTable ->
      case HM.lookup (Wai.requestMethod request) methodTable of
        Nothing -> MethodNotSupportedResult (sort (HM.keys methodTable))
        Just handler -> HandlerResult handler

lookupMethodTable ::
     Router handler -> Wai.Request -> Maybe (MethodsToHandlers handler)
lookupMethodTable (Router handler) request =
  handler <$> U.fromRelativeURI (U.RelativeURI $ Wai.pathInfo request)

isHandlerResult :: Result h -> Bool
isHandlerResult (HandlerResult _) = True
isHandlerResult _ = False

isResourceNotFoundResult :: Result h -> Bool
isResourceNotFoundResult ResourceNotFoundResult = True
isResourceNotFoundResult _ = False

isMethodNotSupportedResult :: Result h -> Bool
isMethodNotSupportedResult (MethodNotSupportedResult _) = True
isMethodNotSupportedResult _ = False

instance Show (Result h) where
  show r =
    case r of
      HandlerResult _ -> "HandlerResult <handler>"
      ResourceNotFoundResult -> "ResourceNotFoundResult"
      MethodNotSupportedResult methods ->
        showString "MethodNotSupportedResult " . showParen True (shows methods) $
        ""
