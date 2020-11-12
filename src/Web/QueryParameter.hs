{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

-- | Reading parameters from URI query.
module Web.QueryParameter
  ( QueryParser
  , parseQuery
  , parseQueryM
  , requireQueryParameter
  , lookupQueryParameter
  , collectQueryParameter
  , lookupRawQueryParameter
  , collectRawQueryParameter
  , Failure(..)
  , QueryParameter(..)
  , CommaSeparatedList(..)
  ) where

import Control.DeepSeq
import Control.Monad
import Control.Monad.Catch
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Data.Bifunctor
import qualified Data.ByteString.Char8 as B
import qualified Data.DList as DL
import Data.Either.Util
import qualified Data.HashMap.Strict as HM
import Data.Int
import Data.Integral.Exact
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Time
import Data.Time.Format.ISO8601
import GHC.Generics
import qualified Network.HTTP.Types as Http
import qualified Web.Exception as E

type Key = B.ByteString

-- | The raw value of a query parameter is an optional bytestring. It
-- may be Nothing if the parameter is specified in the form @?a@
-- rather than @?a=1@ or @?a=@.
type RawValue = Maybe B.ByteString

-- | The query parser functor focusing on performance. It allows
-- parsing query parameters in the applicative style. It traverses the
-- query once only and stops as soon as all interesting parameters are
-- found.
data QueryParser a =
  QueryParser
    { qKeys :: !(DL.DList (Key, SearchType))
    , qParameterReader :: !(ParameterReader a)
    , qMayScanPartially :: !Bool
    }

data SearchType
  = FindFirst
  | FindAll

type ParameterReader = ReaderT ParameterMap (Either Failure)

-- The map contains keys to be searched for.
type ParameterMap = HM.HashMap Key Searched

data Searched
  = SearchedOne
  | FoundOne !RawValue
  | Collected !(DL.DList RawValue)

instance Functor QueryParser where
  fmap f parser = parser {qParameterReader = f <$> qParameterReader parser}

instance Applicative QueryParser where
  pure v =
    QueryParser
      {qKeys = DL.empty, qParameterReader = pure v, qMayScanPartially = True}
  pf <*> px =
    QueryParser
      { qKeys = qKeys pf <> qKeys px
      , qParameterReader = qParameterReader pf <*> qParameterReader px
      , qMayScanPartially = qMayScanPartially pf && qMayScanPartially px
      }

data Failure
  = MissingKey Key
  | BadValue Key RawValue
  deriving (Eq, Show, Generic, NFData)

-- | Runs the query parser on the given query.
parseQuery :: Http.Query -> QueryParser a -> Either Failure a
parseQuery items QueryParser {..} = runReaderT qParameterReader resultingMap
  where
    resultingMap
      | qMayScanPartially =
        scanQueryPartially items (length initialMap) initialMap
      | otherwise = scanFullQuery items initialMap
    initialMap =
      HM.fromListWith mergeInitialLeaves . map (second initialLeafForSearchType) $
      DL.toList qKeys
    mergeInitialLeaves new old
      | Collected {} <- new = new
      | otherwise = old
    initialLeafForSearchType FindFirst = SearchedOne
    initialLeafForSearchType FindAll = Collected DL.empty

scanQueryPartially :: Http.Query -> Int -> ParameterMap -> ParameterMap
scanQueryPartially _ 0 pmap = pmap
scanQueryPartially [] _ pmap = pmap
scanQueryPartially ((k, v):items') remainingCount pmap =
  mergeValue skip save (HM.lookup k pmap) v
  where
    skip = scanQueryPartially items' remainingCount pmap
    save value =
      scanQueryPartially items' (pred remainingCount) (HM.insert k value pmap)

scanFullQuery :: Http.Query -> ParameterMap -> ParameterMap
scanFullQuery [] pmap = pmap
scanFullQuery ((k, v):items') pmap = mergeValue skip save (HM.lookup k pmap) v
  where
    skip = scanFullQuery items' pmap
    save value = scanFullQuery items' (HM.insert k value pmap)

mergeValue :: a -> (Searched -> a) -> Maybe Searched -> RawValue -> a
mergeValue skip save lookedUp newValue =
  case lookedUp of
    Just SearchedOne -> save $ FoundOne newValue
    Just (FoundOne _) -> skip
    Just (Collected oldValues) ->
      save . Collected $ oldValues `DL.snoc` newValue
    Nothing -> skip

-- | Runs 'parseQuery' and throws 'BadExceptionRequest' in case of
-- parse failure.
parseQueryM :: MonadThrow m => Http.Query -> QueryParser a -> m a
parseQueryM query parser =
  either (throwM . formatException) pure $ parseQuery query parser
  where
    formatException (MissingKey key) =
      E.IncorrectParameterException $
      "Parameter '" <>
      T.decodeLatin1 key <> "' is missing from the request query"
    formatException (BadValue key value) =
      E.IncorrectParameterException $
      mconcat
        [ "Wrong value of parameter '"
        , T.decodeLatin1 key
        , "': '"
        , T.decodeLatin1 (fromMaybe "<missing>" value)
        , "'"
        ]

-- | Finds a raw value for the given key. If none found, returns Nothing.
lookupRawQueryParameter :: Key -> QueryParser (Maybe RawValue)
lookupRawQueryParameter key =
  QueryParser
    { qKeys = DL.singleton (key, FindFirst)
    , qParameterReader = ReaderT $ Right . (searchedToMaybe <=< HM.lookup key)
    , qMayScanPartially = True
    }

searchedToMaybe :: Searched -> Maybe RawValue
searchedToMaybe SearchedOne = Nothing
searchedToMaybe (FoundOne v) = Just v
searchedToMaybe (Collected list) = listToMaybe $ DL.toList list

collectQueryParameter :: QueryParameter a => Key -> QueryParser [a]
collectQueryParameter key = collectRawQueryParameter key `bindReader` f
  where
    f = lift . mapM (parseQueryParameterE key)

collectRawQueryParameter :: Key -> QueryParser [RawValue]
collectRawQueryParameter key =
  QueryParser
    { qKeys = DL.singleton (key, FindAll)
    , qParameterReader =
        ReaderT $ Right . (searchedToList <=< maybeToList . HM.lookup key)
    , qMayScanPartially = False
    }

searchedToList :: Searched -> [RawValue]
searchedToList SearchedOne = []
searchedToList (FoundOne v) = [v]
searchedToList (Collected list) = DL.toList list

-- | Finds a value for the given key and tries to parse it. If none
-- found, returns Nothing. If a wrong value is found, generates a
-- failure.
lookupQueryParameter :: QueryParameter a => Key -> QueryParser (Maybe a)
lookupQueryParameter key = lookupRawQueryParameter key `bindReader` (lift . f)
  where
    f Nothing = Right Nothing
    f (Just rawValue) = Just <$> parseQueryParameterE key rawValue

-- | Finds a value for the given key and tries to parse it. If none
-- found or parsing failed, generates a failure.
requireQueryParameter :: QueryParameter a => Key -> QueryParser a
requireQueryParameter key = lookupRawQueryParameter key `bindReader` (lift . f)
  where
    f Nothing = Left (MissingKey key)
    f (Just rawValue) = parseQueryParameterE key rawValue

bindReader :: QueryParser a -> (a -> ParameterReader b) -> QueryParser b
bindReader parser f = parser {qParameterReader = qParameterReader parser >>= f}

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
  parseQueryParameter = (readExactIntegral . B.unpack =<<)

instance QueryParameter Int32 where
  parseQueryParameter = (readExactIntegral . B.unpack =<<)

instance QueryParameter Day where
  parseQueryParameter = (iso8601ParseM . B.unpack =<<)

instance QueryParameter T.Text where
  parseQueryParameter = (eitherToMaybe . T.decodeUtf8' =<<)

-- | A wrapper over a list of values that parses from a
-- comma-separated list parameter. Note that parsing values containing
-- commas is not possible, all commas are considered as separators
-- between values.
newtype CommaSeparatedList a =
  CommaSeparatedList
    { getCommaSeparatedList :: [a]
    }

instance QueryParameter a => QueryParameter (CommaSeparatedList a) where
  parseQueryParameter =
    (fmap CommaSeparatedList . mapM (parseQueryParameter . Just) . B.split ',' =<<)
