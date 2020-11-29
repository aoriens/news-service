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

import qualified Data.HashMap.Strict as HM
import Data.List hiding (delete)
import qualified Network.HTTP.Types as Http
import qualified Web.AppURI as U
import Web.Application

-- | The router type is responsible for finding handlers for the given
-- URI paths and HTTP methods and for handling some exceptional cases.
-- It is parameterized with the request handler type.
newtype Router handler =
  Router (U.AppURI -> MethodsToHandlers handler)

type MethodsToHandlers = HM.HashMap Http.Method

data Method handler =
  Method Http.Method handler

execMethodsSpec :: [Method handler] -> MethodsToHandlers handler
execMethodsSpec = foldl' f HM.empty
  where
    f table (Method m h) = HM.insertWith (\_ _ -> failure m) m h table
    failure k = error $ "Duplicate entry for path " ++ show k

{- |

Creates a router.

> new $ \uri -> case uri of
>   UserURI userId ->
>     [ get $ handleGetForUserId userId
>     , put $ handlePutForUserId userId
>     ]
>   UsersURI ->
>     [post handlePostForUser]

If the found entry does not contain a subentry for the request method,
it is reported as 'MethodNotSupportedResult'.

-}
new :: (U.AppURI -> [Method handler]) -> Router handler
new f = Router $ execMethodsSpec . f

-- | Sets a handler for the specified HTTP method.
method :: Http.Method -> handler -> Method handler
method = Method

get, post, put, delete, patch :: handler -> Method handler
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
route :: Router handler -> Request -> Result handler
route r request =
  case lookupMethodTable r request of
    Nothing -> ResourceNotFoundResult
    Just methodTable ->
      case HM.lookup (requestMethod request) methodTable of
        Nothing
          | HM.null methodTable -> ResourceNotFoundResult
          | otherwise -> MethodNotSupportedResult (sort (HM.keys methodTable))
        Just handler -> HandlerResult handler

lookupMethodTable ::
     Router handler -> Request -> Maybe (MethodsToHandlers handler)
lookupMethodTable (Router handler) request =
  handler <$> U.fromRelativeURI (U.RelativeURI $ requestPathInfo request)

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
