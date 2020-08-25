{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | The module describes a path-driven routing machinery of the web server.
module Web.Router
  ( Router
  , new
  , URLPath
  , Spec
  , ifPath
  , ifAppURL
  , ifMethod
  , route
  , Result(..)
  , isHandlerResult
  , isResourceNotFoundResult
  , isMethodNotSupportedResult
  ) where

import Control.Monad.State.Strict
import Control.Monad.Writer.Strict
import Data.Bifunctor
import qualified Data.HashMap.Strict as HM
import Data.List
import qualified Data.Text as T
import qualified Network.HTTP.Types as Http
import qualified Network.Wai as Wai
import qualified Web.AppURL as U
import Web.Types

type URLPath = [T.Text]

-- | The router type is responsible for finding handlers for the given
-- URL path and HTTP methods and for handling some exceptional cases.
data Router =
  Router !URLTable (Maybe AppURLHandler)

type URLTable = HM.HashMap URLPath MethodsToHandlers

type AppURLHandler = U.AppURL -> MethodsToHandlers

type MethodsToHandlers = HM.HashMap Http.Method EApplication

-- | A monad type to make it easier to specify route configuration.
newtype Spec a =
  Spec (State (URLTable, Maybe AppURLHandler) a)
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

-- | Creates a router. It is possible to use Spec monad for path
-- configuration:
--
-- @
-- new $ do
--   ifPath ["path", "to", "resource"] $ do
--     ifMethod \"GET\"    handleGetForPath
--     ifMethod \"POST\"   handlePostForPath
--     ifMethod \"PUT\"    handlePutForPath
--   ifPath ["another", "path"] $ do
--     ifMethod \"GET\"    handleGetForPath2
--     ifMethod \"DELETE\" handleDeleteForPath2
-- @
new :: Spec () -> Router
new (Spec m) = uncurry Router $ execState m (HM.empty, Nothing)

ifPath :: URLPath -> MethodsSpec () -> Spec ()
ifPath path methodsSpec
  | HM.null methodsToHandlers = error ("Empty entry for path " ++ show path)
  | otherwise =
    Spec . modify' . first $
    HM.insertWith
      (\_ _ -> error ("Duplicate entry for path " ++ show path))
      path
      methodsToHandlers
  where
    methodsToHandlers = execMethodsSpec methodsSpec

ifAppURL :: (U.AppURL -> MethodsSpec ()) -> Spec ()
ifAppURL f =
  Spec . modify' . second $
  maybe (Just appURLHandler) (const errorDuplicateEntry)
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
route (Router urlTable optAppURLHandler) request
  | (Just appURLHandler) <- optAppURLHandler =
    case lookupByAppURL request appURLHandler of
      ResourceNotFoundResult -> lookupByPath request urlTable
      r -> r
  | otherwise = lookupByPath request urlTable

lookupByAppURL :: Wai.Request -> AppURLHandler -> Result
lookupByAppURL request appURLHandler =
  case U.fromRelativeURL . U.RelativeURL $ Wai.pathInfo request of
    Just appURL -> lookupByMethod request $ appURLHandler appURL
    Nothing -> ResourceNotFoundResult

lookupByPath :: Wai.Request -> URLTable -> Result
lookupByPath request urlTable =
  case HM.lookup (Wai.pathInfo request) urlTable of
    Nothing -> ResourceNotFoundResult
    Just subtable -> lookupByMethod request subtable

lookupByMethod :: Wai.Request -> MethodsToHandlers -> Result
lookupByMethod request subtable =
  case HM.lookup (Wai.requestMethod request) subtable of
    Just handler -> HandlerResult handler
    Nothing -> MethodNotSupportedResult (sort (HM.keys subtable))

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
