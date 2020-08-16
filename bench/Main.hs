module Main
  ( main
  ) where

import Criterion.Main
import qualified Web.QueryParameterBench

main :: IO ()
main = defaultMain [Web.QueryParameterBench.run]
