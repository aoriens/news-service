{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE BangPatterns #-}

-- | The module describes a path-driven routing machinery of the web server.
module Web.Router
  ( Router
  , new
  , Spec
  , ifPath
  , ifPathPrefix
  , ifAppURL
  , ifMethod
  , route
  , Result(..)
  , isHandlerResult
  , isResourceNotFoundResult
  , isMethodNotSupportedResult
  ) where

import Control.Applicative
import Control.Monad.State.Strict
import Control.Monad.Writer.Strict
import qualified Data.HashMap.Strict as HM
import Data.List
import qualified Data.Text as T
import qualified Network.HTTP.Types as Http
import qualified Network.Wai as Wai
import qualified Web.AppURL as U
import Web.Types

-- | The router type is responsible for finding handlers for the given
-- URL paths and HTTP methods and for handling some exceptional cases.
data Router =
  Router
    { rPathTrie :: !PathTrie
    , rAppURLHandler :: !(Maybe AppURLHandler)
    }

data PathTrie =
  PathTrie
    { trieSubtries :: !(HM.HashMap PathComponent PathTrie)
    , trieMethodTableForExactMatch :: !MethodsToHandlers
    , trieMethodTableForPrefixMatch :: !MethodsToHandlers
    }

type PathComponent = T.Text

type AppURLHandler = U.AppURL -> MethodsToHandlers

type MethodsToHandlers = HM.HashMap Http.Method EApplication

type Path = [PathComponent]

-- | A monad type to make it easier to specify route configuration.
newtype Spec a =
  Spec (State Router a)
  deriving (Functor, Applicative, Monad)

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
      (\path _ _ -> error $ "Duplicate entry for path " ++ show path)
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
>   ifPath ["path", "to", "resource"] $ do
>     ifMethod "GET"    handleGetForPath
>     ifMethod "POST"   handlePostForPath
>     ifMethod "PUT"    handlePutForPath
>   ifPath ["another", "path"] $ do
>     ifMethod "GET"    handleGetForPath2
>     ifMethod "DELETE" handleDeleteForPath2
>   ifPathPrefix ["user"] $ do
>     ifMethod "GET"    handleGetUser

Each 'ifPath' clause must be passed a unique path, as well as
'ifPathPrefix'. But using the same path in both 'ifPath' and
'ifPathPrefix' is allowed. Path prefixes are allowed to be prefixes of
other path prefixes:

> new $ do
>   ifPathPrefix ["a"] $ ...
>   ifPathPrefix ["a", "b"] $ ... -- OK to have ifPathPrefix ["a", "b"], although ["a"] is its prefix
>   ifPath       ["a", "b"] $ ... -- OK: same path for ifPath & ifPathPrefix
>   ifPathPrefix ["a", "b"] $ ... -- Error: duplicate prefix
>   ifPath       ["q"] $ ...      -- OK
>   ifPath       ["q"] $ ...      -- Error: duplicate path

When matching, an 'ifPath' entry is always preferred over
'ifPathPrefix' entries, and 'ifPathPrefix' entry with the largest
number of path components in the argument is preferred over entries
with shorter ones. If the found entry does not contain a subentry for
the request method, it is reported as 'MethodNotSupportedResult' and
no attempt to find other prefix matches is performed. In the following
example:

> new $ do
>   ifPath ["a", "b", "c"]  $ ifMethod "GET"  h1
>   ifPathPrefix ["a", "b"] $ ifMethod "POST" h2
>   ifPathPrefix ["a"]      $ ifMethod "GET"  h3

- @GET "\/a\/b\/c"@ will match @h1@
- @POST "\/a\/b\/c"@ will result in 'MethodNotSupportedResult', although @h2@
  also supports @POST \/a\/b\/*"@.
- @POST "\/a\/b"@, @POST "\/a\/b\/c\/d"@, and @POST "\/a\/b\/k"@ will
  match @h2@
- @GET "\/a"@ and @GET "\/a\/q"@ will match @h3@

-}
new :: Spec () -> Router
new (Spec m) =
  execState m Router {rPathTrie = emptyPathTrie, rAppURLHandler = Nothing}

emptyPathTrie :: PathTrie
emptyPathTrie =
  PathTrie
    { trieSubtries = mempty
    , trieMethodTableForExactMatch = mempty
    , trieMethodTableForPrefixMatch = mempty
    }

ifPath :: Path -> MethodsSpec () -> Spec ()
ifPath path methodsSpec
  | HM.null methodsToHandlers = error ("Empty entry for path " ++ show path)
  | otherwise =
    Spec . modify' $ \r@Router {..} ->
      r {rPathTrie = insertExactMatch path methodsToHandlers rPathTrie}
  where
    methodsToHandlers = execMethodsSpec methodsSpec

insertExactMatch :: Path -> MethodsToHandlers -> PathTrie -> PathTrie
insertExactMatch path methodTable = insert_ path
  where
    insert_ [] t@PathTrie {..}
      | HM.null trieMethodTableForExactMatch =
        t {trieMethodTableForExactMatch = methodTable}
      | otherwise = error $ "Duplicate entry for path " ++ show path
    insert_ (k:ks) t@PathTrie {..} =
      t
        { trieSubtries =
            HM.insertWith
              (\_new -> insert_ ks)
              k
              (insert_ ks emptyPathTrie)
              trieSubtries
        }

-- | Performs path prefix matching. If matching succeeds, the handler
-- will be passed a request with the path prefix removed from
-- 'Wai.pathInfo'.
ifPathPrefix :: Path -> MethodsSpec () -> Spec ()
ifPathPrefix prefix methodsSpec
  | null prefix = error "Empty path prefix"
  | HM.null methodsToHandlers =
    error ("Empty entry for path prefix " ++ show prefix)
  | otherwise =
    Spec . modify' $ \r@Router {..} ->
      r {rPathTrie = insertPrefixMatch prefix methodsToHandlers rPathTrie}
  where
    methodsToHandlers = execMethodsSpec methodsSpec

insertPrefixMatch :: Path -> MethodsToHandlers -> PathTrie -> PathTrie
insertPrefixMatch path methodTable = insert_ path
  where
    insert_ [] t@PathTrie {..}
      | HM.null trieMethodTableForPrefixMatch =
        t {trieMethodTableForPrefixMatch = methodTable}
      | otherwise = error $ "Duplicate entry for path prefix " ++ show path
    insert_ (k:ks) t@PathTrie {..} =
      t
        { trieSubtries =
            HM.insertWith
              (\_new -> insert_ ks)
              k
              (insert_ ks emptyPathTrie)
              trieSubtries
        }

ifAppURL :: (U.AppURL -> MethodsSpec ()) -> Spec ()
ifAppURL f =
  Spec . modify' $ \r@Router {..} ->
    r
      { rAppURLHandler =
          maybe (Just appURLHandler) (const errorDuplicateEntry) rAppURLHandler
      }
  where
    appURLHandler = execMethodsSpec . f
    errorDuplicateEntry = error "ifAppURL clause is used more than once"

ifMethod :: Http.Method -> EApplication -> MethodsSpec ()
ifMethod method handler =
  MethodsSpec . tell . MethodsToHandlersMonoid $ HM.singleton method handler

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
route r@Router {..} request =
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
lookupMethodTable Router {..} request = do
  (lookupByAppURL request =<< rAppURLHandler) <|> lookupByPath rPathTrie request

lookupByAppURL :: Wai.Request -> AppURLHandler -> Maybe PathMatch
lookupByAppURL request appURLHandler =
  (`PathMatch` Nothing) . appURLHandler <$>
  U.fromRelativeURL (U.RelativeURL $ Wai.pathInfo request)

lookupByPath :: PathTrie -> Wai.Request -> Maybe PathMatch
lookupByPath trie request = go Nothing trie (Wai.pathInfo request)
  where
    go !nearestPrefixMatch t []
      | HM.null $ trieMethodTableForExactMatch t = nearestPrefixMatch
      | otherwise = Just $! PathMatch (trieMethodTableForExactMatch t) Nothing
    go !nearestPrefixMatch oldTrie (k:suffix)
      | Just newTrie <- HM.lookup k $ trieSubtries oldTrie =
        go
          (if null $ trieMethodTableForPrefixMatch newTrie
             then nearestPrefixMatch
             else Just $!
                  PathMatch (trieMethodTableForPrefixMatch newTrie) $
                  Just suffix)
          newTrie
          suffix
      | otherwise = nearestPrefixMatch

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
