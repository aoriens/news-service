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
import qualified Data.Text as T
import qualified Network.HTTP.Types as Http
import qualified Network.Wai as Wai
import qualified Web.AppURI as U
import Web.Types

-- | The router type is responsible for finding handlers for the given
-- URI paths and HTTP methods and for handling some exceptional cases.
newtype Router =
  Router (Maybe AppURIHandler)

type PathComponent = T.Text

type AppURIHandler = U.AppURI -> MethodsToHandlers

type MethodsToHandlers = HM.HashMap Http.Method EApplication

type Path = [PathComponent]

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

Creates a router. It is possible to use Spec monad for path
configuration:

> new $ do
>   path ["path", "to", "resource"] $ do
>     get    handleGetForPath
>     post   handlePostForPath
>     put    handlePutForPath
>   path ["another", "path"] $ do
>     get    handleGetForPath2
>     delete handleDeleteForPath2
>   pathPrefix ["user"] $ do
>     get    handleGetUser

Each 'path' clause must be passed a unique path, as well as
'pathPrefix'. But using the same path in both 'path' and 'pathPrefix'
is allowed. Path prefixes are allowed to be prefixes of other path
prefixes:

> new $ do
>   pathPrefix ["a"] $ ...
>   pathPrefix ["a", "b"] $ ... -- OK to have pathPrefix ["a", "b"], although ["a"] is its prefix
>   path       ["a", "b"] $ ... -- OK: same path for path & pathPrefix
>   pathPrefix ["a", "b"] $ ... -- Error: duplicate prefix
>   path       ["q"] $ ...      -- OK
>   path       ["q"] $ ...      -- Error: duplicate path

When matching, a 'path' entry is always preferred over 'pathPrefix'
entries, and the longest (in the number of components) 'pathPrefix'
entry is preferred over shorter ones. If the found entry does not
contain a subentry for the request method, it is reported as
'MethodNotSupportedResult' and no attempt to find other prefix matches
is performed. In the following example:

> new $ do
>   path ["a", "b", "c"]  $ get  h1
>   pathPrefix ["a", "b"] $ post h2
>   pathPrefix ["a"]      $ get  h3

- @GET "\/a\/b\/c"@ will match @h1@
- @POST "\/a\/b\/c"@ will result in 'MethodNotSupportedResult',
  although @h2@ also supports @POST \/a\/b\/*"@.
- @POST "\/a\/b"@, @POST "\/a\/b\/c\/d"@, and @POST "\/a\/b\/k"@ will
  match @h2@
- @GET "\/a"@ and @GET "\/a\/q"@ will match @h3@

-}
new :: (U.AppURI -> MethodsSpec ()) -> Router
new f = Router $ Just (execMethodsSpec . f)

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
    Just (PathMatch methodTable optNewPath) ->
      case (HM.lookup (Wai.requestMethod request) methodTable, optNewPath) of
        (Nothing, _) -> MethodNotSupportedResult (sort (HM.keys methodTable))
        (Just handler, Nothing) -> HandlerResult handler
        (Just handler, Just newPath) ->
          HandlerResult $ \session req ->
            handler session req {Wai.pathInfo = newPath}

lookupMethodTable :: Router -> Wai.Request -> Maybe PathMatch
lookupMethodTable (Router handler) request = lookupByAppURI request =<< handler

lookupByAppURI :: Wai.Request -> AppURIHandler -> Maybe PathMatch
lookupByAppURI request appURIHandler =
  (`PathMatch` Nothing) . appURIHandler <$>
  U.fromRelativeURI (U.RelativeURI $ Wai.pathInfo request)

data PathMatch =
  PathMatch
    !MethodsToHandlers
    !(Maybe Path) -- ^ A new path info to set to the request for
                  -- passing to the handler

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
