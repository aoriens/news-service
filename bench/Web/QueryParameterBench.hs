{-# LANGUAGE TupleSections #-}

module Web.QueryParameterBench
  ( run
  ) where

import Control.Applicative
import Criterion.Main
import qualified Data.ByteString.Char8 as BS8
import Data.List
import Web.QueryParameter

run :: Benchmark
run =
  bgroup
    "Web.QueryParameters"
    [ bgroup
        "Prelude.lookup"
        [ bench "OK, 1 parameter, 0 items to skip" $
          nf (lookup "a") $ makeQuery 0 1
        , bench "OK, 1 parameter, 10 items to skip" $
          nf (lookup "a") $ makeQuery 10 1
        , bench "OK, 2 parameters, 0 items to skip" $
          nf (liftA2 (,) (lookup "a") (lookup "b")) $ makeQuery 0 2
        , bench "OK, 2 parameters, 10 items to skip" $
          nf (liftA2 (,) (lookup "a") (lookup "b")) $ makeQuery 10 2
        , bench "Not found 2 parameters, 10 items to skip" $
          nf (liftA2 (,) (lookup "a") (lookup "b")) $ makeQuery 10 0
        ]
    , bgroup
        "Web.QueryParameters.lookupRawP"
        [ bench "OK, 1 parameter, 0 items to skip" $
          nf (`parseQuery` lookupRawP "a") $ makeQuery 0 1
        , bench "OK, 1 parameter, 10 items to skip" $
          nf (`parseQuery` lookupRawP "a") $ makeQuery 10 1
        , bench "OK, 2 parameters, 0 items to skip" $
          nf (`parseQuery` liftA2 (,) (lookupRawP "a") (lookupRawP "b")) $
          makeQuery 0 2
        , bench "OK, 2 parameters, 10 items to skip" $
          nf (`parseQuery` liftA2 (,) (lookupRawP "a") (lookupRawP "b")) $
          makeQuery 10 2
        , bench "Not found 2 parameters, 10 items to skip" $
          nf (`parseQuery` liftA2 (,) (lookupRawP "a") (lookupRawP "b")) $
          makeQuery 10 0
        ]
    ]

makeQuery :: Int -> Int -> [(BS8.ByteString, Maybe BS8.ByteString)]
makeQuery numDummies numKeys =
  queryItems $
  take numDummies dummyKeys ++
  take numKeys ["offset", "limit", "size", "length"]
  where
    dummyKeys =
      unfoldr (\n -> Just (BS8.pack ("dummy" ++ show n), succ n)) (0 :: Int)

queryItems :: [BS8.ByteString] -> [(BS8.ByteString, Maybe BS8.ByteString)]
queryItems = map (, Just "")
