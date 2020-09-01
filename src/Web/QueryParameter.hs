{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

-- | Reading parameters from URI query.
module Web.QueryParameter
  ( QueryParser
  , parseQuery
  , parseQueryM
  , require
  , lookup
  , lookupRaw
  , Failure(..)
  , QueryParameter(..)
  ) where

import Control.DeepSeq
import Control.Monad
import Control.Monad.Catch
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.DList as DL
import qualified Data.HashMap.Strict as HM
import Data.Int
import Data.Integral.Exact
import Data.Maybe
import qualified Data.Text.Encoding as T
import GHC.Generics
import qualified Network.HTTP.Types as Http
import Prelude hiding (lookup)
import qualified Web.Exception as E

type Key = BS.ByteString

-- | The raw value of a query parameter is an optional bytestring. It
-- may be Nothing if the parameter is specified in the form @?a@
-- rather than @?a=1@ or @?a=@.
type RawValue = Maybe BS.ByteString

-- | The query parser functor focusing on performance. It allows
-- parsing query parameters in the applicative style. It traverses the
-- query once only and stops as soon as all interesting parameters are
-- found.
data QueryParser a =
  QueryParser
    (DL.DList Key) -- ^ Keys to search for
    (ParameterReader a) -- ^ A reader monad to consume parameters found

type ParameterReader = ReaderT ParameterMap (Either Failure)

-- The map contains keys to be searched for. Values may be:
-- - Nothing - no value is found for the key yet
-- - Just value - a value is found
type ParameterMap = HM.HashMap Key (Maybe RawValue)

instance Functor QueryParser where
  fmap f (QueryParser keys r) = QueryParser keys (fmap f r)

instance Applicative QueryParser where
  pure = QueryParser DL.empty . pure
  (QueryParser keys1 f) <*> (QueryParser keys2 a) =
    QueryParser (keys1 <> keys2) (f <*> a)

data Failure
  = MissingKey Key
  | BadValue Key RawValue
  deriving (Eq, Show, Generic, NFData)

-- | Runs the query parser on the given query.
parseQuery :: Http.Query -> QueryParser a -> Either Failure a
parseQuery items (QueryParser keys r) =
  runReaderT r $ findAll initialMap (length initialMap) items
  where
    initialMap = HM.fromList . map (, Nothing) $ DL.toList keys
    findAll pmap 0 _ = pmap
    findAll pmap _ [] = pmap
    findAll pmap remainingCount ((k, v):items') =
      case HM.lookup k pmap
        -- The key is searched for, but no value found yet
            of
        Just Nothing ->
          findAll (HM.insert k (Just v) pmap) (pred remainingCount) items'
        -- The key is not searched for, or a value has already been found
        _ -> findAll pmap remainingCount items'

-- | Runs 'parseQuery' and throws 'BadExceptionRequest' in case of
-- parse failure.
parseQueryM :: MonadThrow m => Http.Query -> QueryParser a -> m a
parseQueryM query parser =
  either (throwM . formatException) pure $ parseQuery query parser
  where
    formatException (MissingKey key) =
      E.BadRequestException $
      "Parameter '" <>
      T.decodeLatin1 key <> "' is missing from the request query"
    formatException (BadValue key value) =
      E.BadRequestException $
      mconcat
        [ "Wrong value of parameter '"
        , T.decodeLatin1 key
        , "': '"
        , T.decodeLatin1 (fromMaybe "<missing>" value)
        , "'"
        ]

-- | Finds a raw value for the given key. If none found, returns Nothing.
lookupRaw :: Key -> QueryParser (Maybe RawValue)
lookupRaw key =
  QueryParser (DL.singleton key) (ReaderT $ pure . join . HM.lookup key)

-- | Finds a value for the given key and tries to parse it. If none
-- found, returns Nothing. If a wrong value is found, generates a
-- failure.
lookup :: QueryParameter a => Key -> QueryParser (Maybe a)
lookup key = mapValue f (lookupRaw key)
  where
    f a =
      case a of
        Nothing -> Right Nothing
        Just optBS -> Just <$> parseQueryParameterE key optBS

-- | Finds a value for the given key and tries to parse it. If none
-- found or parsing failed, generates a failure.
require :: QueryParameter a => Key -> QueryParser a
require key = mapValue f (lookupRaw key)
  where
    f a =
      case a of
        Nothing -> Left (MissingKey key)
        Just optBS -> parseQueryParameterE key optBS

mapValue :: (a -> Either Failure b) -> QueryParser a -> QueryParser b
mapValue f (QueryParser keys r) = QueryParser keys (r >>= lift . f)

parseQueryParameterE :: QueryParameter a => Key -> RawValue -> Either Failure a
parseQueryParameterE key bs =
  case parseQueryParameter bs of
    Just v -> Right v
    Nothing -> Left (BadValue key bs)

-- | Types that can be parsed from an HTTP request query item.
class QueryParameter a where
  parseQueryParameter :: RawValue -> Maybe a -- ^ Parses a value from an optional bytestring value. It should
  -- return Nothing in case of parse error.

-- | Parsing always succeeds. It may be used to check for parameter
-- existence.
instance QueryParameter () where
  parseQueryParameter _ = Just ()

instance QueryParameter Int where
  parseQueryParameter = (>>= readExactIntegral . BS8.unpack)

instance QueryParameter Int32 where
  parseQueryParameter = (>>= readExactIntegral . BS8.unpack)
