{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | The module describes a path-driven routing machinery of the web server.
module Web.Router
  ( Router
  , new
  , UrlPath
  , Spec
  , ifPath
  , ifMethod
  , route
  , Result(..)
  , isHandlerResult
  , isResourceNotFoundResult
  , isMethodNotSupportedResult
  ) where

import Control.Monad.State
import qualified Data.HashMap.Strict as HM
import Data.List
import qualified Data.Text as T
import qualified Network.HTTP.Types as Http
import qualified Network.Wai as Wai

type UrlPath = [T.Text]

-- | The router type is responsible for finding handlers for the given
-- URL path and HTTP methods and for handling some exceptional cases.
newtype Router =
  Router Table

type Table = HM.HashMap UrlPath MethodsToHandlers

type MethodsToHandlers = HM.HashMap Http.Method Wai.Application

-- | A monad type to make it easier to specify route configuration.
newtype Spec a =
  Spec (State Table a)
  deriving (Functor, Applicative, Monad)

newtype MethodsSpec a =
  MethodsSpec (State MethodsToHandlers a)
  deriving (Functor, Applicative, Monad)

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
new (Spec m) = Router $ execState m HM.empty

ifPath :: UrlPath -> MethodsSpec () -> Spec ()
ifPath path (MethodsSpec methodsSpec) =
  Spec $
  if HM.null methodsToHandlers
    then error ("Empty entry for path " ++ show path)
    else modify' $
         HM.insertWith
           (\_ _ -> error ("Duplicate entry for path " ++ show path))
           path
           methodsToHandlers
  where
    methodsToHandlers = execState methodsSpec HM.empty

ifMethod :: Http.Method -> Wai.Application -> MethodsSpec ()
ifMethod method handler =
  MethodsSpec . modify' $
  HM.insertWith
    (\_ _ -> error ("Duplicate handler for method " ++ show method))
    method
    handler

-- | The result of finding a route.
data Result
  -- | A handler is found for the specified request.
  = HandlerResult Wai.Application
  -- | No handler is found. This is typically output as HTTP 404.
  | ResourceNotFoundResult
  -- | The requested resource does not support a specified method. The
  -- parameter contains known, sorted methods. This is typically
  -- output as HTTP 405 with Allow header.
  | MethodNotSupportedResult [Http.Method]

-- | Find a handler for the specified request.
route :: Router -> Wai.Request -> Result
route (Router table) request =
  case HM.lookup (Wai.pathInfo request) table of
    Nothing -> ResourceNotFoundResult
    Just subtable ->
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
