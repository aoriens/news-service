{-# LANGUAGE OverloadedStrings #-}

module Main
  ( main
  ) where

import qualified Data.ByteString.Lazy.Char8 as BS
import qualified Network.HTTP.Types.Header as Http
import qualified Network.HTTP.Types.Status as Http
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp

main :: IO ()
main = do
  putStrLn "Server started"
  Warp.run 4000 application

application :: Wai.Application
application request respond =
  respond $
  Wai.responseLBS Http.ok200 [(Http.hContentType, "text/plain")] $
  "It works!\n" <> BS.pack (show request)
